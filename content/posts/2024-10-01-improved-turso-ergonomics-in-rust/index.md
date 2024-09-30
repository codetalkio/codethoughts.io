+++
title = "Improved Turso (libsql) ergonomics in Rust"
date = 2024-10-01

[taxonomies]
tags = ["rust", "turso", "macros", "sql"]

+++
I recently started using [Turso](https://turso.tech), which is a great database option for serverless as well as to keep costs low. Its pricing is more than competitive with something like [AWS DynamoDB](https://aws.amazon.com/dynamodb/pricing/on-demand/), which is quite interesting as a alternative to it.

One problem though, there's no support by any good ORMs or similar in the Rust ecosystem for [Turso](https://turso.tech).

Instead, I was currently writing something like this, just to get a `User` out from the database via their session:

<div></div><!-- more -->

```rust
let database_context = ctx.data::<DatabaseContext>().expect("Missing database context");
let conn = database_context.users.get().expect("No connection to users database");
let mut stmt = conn
    .prepare(
        "SELECT users.user_id, users.first_name, users.last_name, users.country, users.onboarding_step, users.demo_user
        FROM auth_sessions AS sessions
        LEFT JOIN users AS users ON users.user_id = sessions.user_id
        WHERE sessions.session_id = :session",
    )
    .await
    .expect("Failed to create the prepared statement to query sessions table");
let row = stmt
    .query(libsql::named_params! { ":session": auth_session.session.clone() })
    .await
    .expect("Failed to query sessions table");
    .next()
    .await
    .expect("Failed to get the first row of the sessions table");

if let Some(row) = row {
    let user: User = de::from_row::<lib_database::User>(&row)
        .expect("Failed to deserialize user")
        .into();
}
```

The above does the following:

- Pulls the connection info from the async-graphql request context
- Establishes a connection to the `users` database
- Creates a prepared statement
- Runs the query with the prepared statement and passes in the named parameters and tries to get the rows
- Tries to get the first row (a `Result<Option<Row>>`)
- And finally, if that row was not `None`, tries to deserialize the row first into the `lib_database::User` type which was manually created, and finally into the `User` GraphQL type, which has a `From` implementation to convert from the database type to the GraphQL type

…quite verbose if I were to be honest, and not something that I'd like to bloat my codebase with.

There are also some obvious issues here:

- Nothing is checking my SQL statement is valid and that those tables/columns even exist
- Nothing verifies that my SQL is getting the necessary data to be able to deserialize into the `lib_database::User` type

{% aside() %}
Sadly, we cannot use [sqlx](https://github.com/launchbadge/sqlx) for Turso. There's an open issue tracking the progress in https://github.com/launchbadge/sqlx/issues/2674.
{% end %}

Instead I'd like something closer to the following, checked at compile time:

- Verifies that the query only refers to tables and columns that exist, by checking against the database schema file (not needing a running database)
- Verify that all named parameters in the query is also populated
- Create some form of an anonymous record/map with the column name as the key for each row

The code could look something like:

```rust
// Construct a type-safe builder based on the parameters in the query.
let query = query!(
    "SELECT users.user_id, users.first_name, users.last_name, users.country, users.onboarding_step, users.demo_user
    FROM auth_sessions
    LEFT JOIN users ON users.user_id = auth_sessions.user_id
    WHERE auth_sessions.session_id = :session"
).session(auth_session.session.clone().into()).build();
// Easily grab the first row.
let row = query.first(&conn).await;
// Access each selected field in a type-safe manner.
let user =
  User::new(row.user_id, row.first_name, row.last_name, row.onboarding_step);
```

So let's build it!

{{ toc() }}

# Compile time checking

We want as much as possible to be checked at compile-time, and that means that we very likely will need to reach for a [Procedural Macro](https://doc.rust-lang.org/reference/procedural-macros.html). In Rust, we have two forms of macros:

- [Declarative macros](https://doc.rust-lang.org/rust-by-example/macros.html): Defined using `macro_rules!` (e.g. `println!`)
- [Procedural macros](https://doc.rust-lang.org/reference/procedural-macros.html): Defined in their own create via `#[proc_macro]` and similar, depending on what type you want (e.g. `custom!(...)`, `#[derive(CustomDerive)]` or `#[CustomAttribute]`)

Proc macros cannot be used in the same crate as they are defined, so we'll first need to create a new crate that we can use with our project:

```bash
$ cargo new lib-macros --lib
```

We'll then need to edit our `lib-macros/Cargo.toml` to add a few crates, both for our proc macro but also for our SQL parsing logic:

```toml ,linenos
[package]
name = "lib-macros"
version = "0.1.0"
edition = "2021"

[lib]
proc-macro = true

[profile.release]
opt-level = 3     # Optimize for speed.
lto = true        # Enable Link Time Optimization.
codegen-units = 1 # Reduce Parallel Code Generation Units to Increase Optimization.
strip = true      # Automatically strip symbols from the binary.
debug = false

[dependencies]
# Parsing SQL.
sqlparser = "0.51.0"
# Macro dependencies.
syn = "2.0"
quote = "1.0"
proc-macro2 = "1.0"

[dev-dependencies]
# Necessary for testing and also for any project actually using this proc-macro.
typed-builder = "0.20.0"
libsql = "0.6.0"
serde = { version = "1.0.209", features = ["serde_derive"] }
serde_json = "1.0.127"
```

Note that when using this proc-macro in your project, you'll basically need all the dependencies that are listed in `[dev-dependencies]`, since the generated code will rely on these.

We'll then tackle the functionality in two steps:

1. Handle parsing SQL from strings and files
2. Stitch things together in a proc-macro that will generate our code

## Parsing SQL

We will be reaching for the [sqlparser](https://crates.io/crates/sqlparser) crate to be able to parse the SQLite table definitions as well as our queries.

Here's a rough overview of what the code does:

- `select_names`: Extract the selected field names from a `SELECT` statement.
- `create_table_columns`: Extract the column names and types from a `CREATE TABLE` statement.
- `process_sql_file`: Read an SQL file, extract all the CREATE TABLE statements, and process them.
- `validate_fields`: Validate that the selected fields are found in the tables and named as expected.

Save the contents below into `lib-macros/src/sql.rs`.

```rust ,linenos
use sqlparser::ast::{
    ColumnOption, ColumnOptionDef, DataType, SelectItem, SetExpr, Statement, TableFactor,
    TableWithJoins,
};
use sqlparser::dialect::SQLiteDialect;
use sqlparser::parser::Parser;
use std::collections::HashMap;
use std::fs;
use std::io;

#[derive(PartialEq, Eq, Debug)]
pub struct TableInfo {
    pub table_name: String,
    pub columns: Vec<(String, String)>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FieldInfo {
    pub table_name: String,
    pub alias: String,
    pub field_name: String,
}

/// Validate that the selected fields are found in the tables and named as expected.
pub fn validate_fields(
    tables: &HashMap<String, TableInfo>,
    fields: &Vec<FieldInfo>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Validate that the fields are found and named as expected.
    for field in fields {
        if !tables.contains_key(&field.table_name) {
            return Err(format!(
                "Table '{}' was not found in the SQL schema file.",
                field.table_name
            )
            .into());
        }
        let table = tables.get(&field.table_name).unwrap();
        if !table.columns.iter().any(|(name, _)| name == &field.field_name) {
            return Err(format!(
                "Field '{}' does not exist in table '{}'.",
                field.field_name, field.table_name
            )
            .into());
        }
    }

    Ok(())
}

/// Process a SQL file and extract the CREATE TABLE statements.
pub fn process_sql_file(filename: &str) -> io::Result<HashMap<String, TableInfo>> {
    // Read the contents of our SQL file.
    let content = fs::read_to_string(filename)?;

    // Split the content on semicolons to get individual statements.
    let statements: Vec<&str> = content.split(';').collect();

    // Filter and process CREATE TABLE statements.
    let mut table_map = HashMap::new();

    for statement in statements {
        let trimmed_stmt = statement.trim();
        if trimmed_stmt.to_lowercase().starts_with("create table") {
            match create_table_columns(trimmed_stmt) {
                Ok(table_info) => {
                    table_map.insert(table_info.table_name.clone(), table_info);
                }
                Err(e) => {
                    eprintln!("Error processing statement: {trimmed_stmt}. Error: {e}");
                }
            }
        }
    }

    Ok(table_map)
}

/// Extract the columns from a CREATE TABLE statement.
///
/// See the test [`test_create_table_columns`] for an example.
pub fn create_table_columns(query: &str) -> Result<TableInfo, Box<dyn std::error::Error>> {
    let dialect = SQLiteDialect {};
    let mut ast = Parser::parse_sql(&dialect, query)?;

    // We expect one statement, a CREATE TABLE
    if ast.len() != 1 {
        return Err("Expected a single SQL statement".into());
    }

    let statement = ast.pop().unwrap();

    // Match the AST to drill into the CreateTable statement.
    if let Statement::CreateTable(create_table) = statement {
        let columns = create_table.columns;
        // Extract column names and data types
        let column_info: Vec<(String, String)> = columns
            .iter()
            .map(|col| {
                let name = col.name.value.clone();
                let data_type = sql_to_rust_type(&col.data_type, &col.options);
                (name, data_type)
            })
            .collect();

        // The table name has the format: ObjectName([Ident { value: "users", quote_style: Some('"') }])
        let table_name =
            create_table.name.0.into_iter().map(|ident| ident.value).collect::<Vec<_>>().join(".");
        Ok(TableInfo { table_name, columns: column_info })
    } else {
        Err("Expected a CreateTable statement".into())
    }
}

/// Convert the SQL data type to the Rust equivalent.
fn sql_to_rust_type(data_type: &DataType, options: &Vec<ColumnOptionDef>) -> String {
    let datatype = match data_type {
        DataType::Int(_) => "i32".to_string(),
        DataType::BigInt(_) => "i64".to_string(),
        DataType::SmallInt(_) => "i16".to_string(),
        DataType::Char(_) => "String".to_string(),
        DataType::Varchar(_) => "String".to_string(),
        DataType::Float(_) => "f32".to_string(),
        DataType::Double => "f64".to_string(),
        DataType::Boolean => "bool".to_string(),
        DataType::Date => "chrono::NaiveDate".to_string(),
        DataType::Time(_, _) => "chrono::NaiveTime".to_string(),
        DataType::Timestamp(_, _) => "chrono::NaiveDateTime".to_string(),
        DataType::Decimal(_) => "rust_decimal::Decimal".to_string(),
        // Add more mappings as needed
        _ => "".to_string(), // Default to String for unknown types
    };

    // Check if the type is optional, indicated by no NOT NULL and no DEFAULT value.
    //
    // Example of optional:
    // options: []
    //
    // Example of NOT NULL:
    // options: [ColumnOptionDef { name: None, option: NotNull }]
    //
    // Example of NOT NULL with DEFAULT:
    // options: [ColumnOptionDef { name: None, option: NotNull }, ColumnOptionDef { name: None, option: Default(Value(Boolean(false))) }]
    let mut has_not_null = false;
    let mut has_default = false;
    for option in options {
        match option {
            ColumnOptionDef { option: ColumnOption::NotNull, .. } => {
                has_not_null = true;
            }
            ColumnOptionDef { option: ColumnOption::Default(_), .. } => {
                has_default = true;
            }
            _ => {}
        }
    }

    if has_not_null || has_default {
        datatype
    } else {
        format!("Option<{datatype}>")
    }
}

/// Extract the table name from a FROM clause.
fn extract_table_name(from: &Vec<TableWithJoins>) -> Option<String> {
    match from.first() {
        Some(table_with_joins) => match &table_with_joins.relation {
            TableFactor::Table { name, .. } => Some(
                name.clone().0.into_iter().map(|ident| ident.value).collect::<Vec<_>>().join("."),
            ),
            _ => None,
        },
        None => None,
    }
}

/// Extract the selected field names from a SELECT statement.
///
/// See the test [`test_select_names`] for an example.
pub fn select_names(sql: &str) -> Result<Vec<FieldInfo>, Box<dyn std::error::Error>> {
    let dialect = SQLiteDialect {};
    let mut ast = Parser::parse_sql(&dialect, sql).unwrap();

    // We expect one statement, a SELECT
    if ast.len() != 1 {
        return Err("Expected a single SQL statement".into());
    }
    let statement = ast.pop().unwrap();

    // Match the AST to drill into the Query and Select statement.
    if let Statement::Query(query) = statement {
        if let SetExpr::Select(select) = *query.body {
            // Extract field names from the projection
            let fields: Vec<FieldInfo> = select
                .projection
                .iter()
                .filter_map(|item| match item {
                    SelectItem::UnnamedExpr(expr) => Some((expr.to_string(), expr.to_string())),
                    SelectItem::ExprWithAlias { expr, alias } => {
                        Some((expr.to_string(), alias.value.clone()))
                    }
                    SelectItem::QualifiedWildcard(object_name, _) => {
                        Some((object_name.to_string(), object_name.to_string()))
                    }
                    SelectItem::Wildcard(_) => Some(("*".to_string(), "*".to_string())),
                })
                // Clean up name if the field name is prefixed with the table name.
                .map(|(name, alias)| {
                    let split_name: Vec<&str> = name.split('.').collect();

                    // Extract the table name from the field, if it's prefixed, and fallback to the
                    // FROM table name.
                    let table_name = if split_name.len() > 1 {
                        name.split('.')
                            .next()
                            .map(|s| s.to_string())
                            .expect("Expected a table name")
                    } else {
                        extract_table_name(&select.from).expect("Expected a table name")
                    };
                    FieldInfo {
                        table_name,
                        alias: alias.split('.').last().map(|s| s.to_string()).unwrap_or(alias),
                        // Either the field name is of format "table.field" or "field".
                        field_name: split_name.last().map(|s| s.to_string()).unwrap_or(name),
                    }
                })
                .collect();

            Ok(fields)
        } else {
            Err("Expected a SELECT statement".into())
        }
    } else {
        Err("Expected a Query statement".into())
    }
}
```

We can add a few tests in the same file, that'll demonstrate and validate our functionality:

```rust ,linenos
#[test]
fn test_create_table_columns() {
    let columns = create_table_columns(
        "
  CREATE TABLE IF NOT EXISTS
  \"users\"
  (
    -- The user id.
    \"user_id\" varchar(36) NOT NULL,
    -- The user's first name.
    \"first_name\" varchar(255),
    -- The user's last name.
    \"last_name\" varchar(255),
    -- The user's country code (e.g. DK, US, etc.).
    \"country\" varchar(3),
    -- If the user is onboarding or not.
    -- Possible values: 'first' | 'second' | 'third' | 'done'
    \"onboarding_step\" varchar(255) NOT NULL DEFAULT 'first',
    -- If the user is a demo user or not.
    \"demo_user\" boolean NOT NULL DEFAULT false,
    -- The time the user was created.
    \"created_at\" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- The time the user was last updated.
    \"updated_at\" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (user_id)
  );
",
    );

    // Convert the expected columns from &str to String.
    let expected_columns = vec![
        ("user_id", "String"),
        ("first_name", "Option<String>"),
        ("last_name", "Option<String>"),
        ("country", "Option<String>"),
        ("onboarding_step", "String"),
        ("demo_user", "bool"),
        ("created_at", "chrono::NaiveDateTime"),
        ("updated_at", "chrono::NaiveDateTime"),
    ]
    .iter()
    .map(|(name, data_type)| (name.to_string(), data_type.to_string()))
    .collect();

    // Validate that the columns found as expected, and have the correct types.
    assert_eq!(
        columns.unwrap(),
        TableInfo { table_name: "users".to_string(), columns: expected_columns }
    );
}

#[test]
fn test_select_names() {
    let names = select_names(
        "
      SELECT
        users.user_id as tmp_users_name,
        users.user_id,
        users.first_name,
        users.last_name,
        users.country,
        users.onboarding_step,
        users.demo_user,
        session_id
      FROM auth_sessions
      LEFT JOIN users ON users.user_id = auth_sessions.user_id
      WHERE auth_sessions.session_id = :session",
    );

    let expected_fields = [
        ("users", "user_id", "tmp_users_name"),
        ("users", "user_id", "user_id"),
        ("users", "first_name", "first_name"),
        ("users", "last_name", "last_name"),
        ("users", "country", "country"),
        ("users", "onboarding_step", "onboarding_step"),
        ("users", "demo_user", "demo_user"),
        ("auth_sessions", "session_id", "session_id"),
    ]
    .map(|(tbl, name, alias)| FieldInfo {
        table_name: tbl.to_string(),
        field_name: name.to_string(),
        alias: alias.to_string(),
    });

    // Validate that the fields are found and named as expected.
    assert_eq!(names.unwrap(), expected_fields)
}

#[test]
fn test_validate_fields() {
    let tables = process_sql_file("../database/users.sql").expect("Failed to process SQL file");
    let valid_fields = vec![
        FieldInfo {
            table_name: "users".to_string(),
            alias: "user_id".to_string(),
            field_name: "user_id".to_string(),
        },
        FieldInfo {
            table_name: "users".to_string(),
            alias: "first_name".to_string(),
            field_name: "first_name".to_string(),
        },
    ];
    // Validate that the fields are found and named as expected.
    assert!(validate_fields(&tables, &valid_fields).is_ok());

    let invalid_fields = vec![
        FieldInfo {
            table_name: "users".to_string(),
            alias: "user_id".to_string(),
            field_name: "user_id".to_string(),
        },
        FieldInfo {
            table_name: "users".to_string(),
            alias: "first_name".to_string(),
            field_name: "first_name_does_not_exist".to_string(),
        },
    ];

    // Validate that we fail when the field name is invalid.
    assert!(validate_fields(&tables, &invalid_fields).is_err());

    let invalid_table_name = vec![FieldInfo {
        table_name: "users_does_not_exist".to_string(),
        alias: "user_id".to_string(),
        field_name: "user_id".to_string(),
    }];

    // Validate that we fail when the table name is invalid.
    assert!(validate_fields(&tables, &invalid_table_name).is_err());
}
```

## Proc Macro

With our helper functions in place for working with the SQL, we can finally piece together our proc-macro.

Here's a rough overview of what the code does:

- Reads the input query as a string literal
- Uses our SQL helpers to
    - Extract the selected fields from the query
    - Get all table information from a file located at `../database/users.sql` (that is, outside of the `lib-macros` directory, and inside another directory called `database`)
    - Validate that the selected fields actually exist in the correct tables
- Extract all the params under the assumption that they are the only things starting with `:`
- Prepare some of the code generation we'll need later
    - Prepare the struct field names for our `QueryParams` typesafe builder, with a type of `libsql::Value`
    - Prepare the parameter name and struct field value pairs for `libsql::named_params!` which is used to pass the parameters to a prepared statement
    - Prepare our struct of our return data based on our selected fields and the types that they have, which is found by looking them up in the table information
- Finally, we put it all together and set up the various structs with their fields, as well as an `impl` of our struct that provides some convenience functions
    - `execute`: Runs the query and deserializes each `libsql::row` into our custom return data struct
    - `first`: Simply returns the first row if there is any, or `None`

Adjust the path to your database schema and save the contents below into `lib-macros/src/lib.rs`.

```rust ,linenos
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, parse_str, Error, LitStr, Type};

mod sql;

#[proc_macro]
pub fn query(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);
    let sql = input.value();

    // Validate that we are only selecting fields that exist in the SQL schema.
    let select_fields =
        sql::select_names(&sql).expect("Unable to extract select fields from SQL input");
    let table_map = sql::process_sql_file("../database/users.sql")
        .expect("Failed to process SQL file '../database/users.sql'");
    let validation = sql::validate_fields(&table_map, &select_fields);

    // If the validation fails, return an nice compiler error.
    if let Err(msg) = validation {
        return Error::new_spanned(input, msg.to_string()).into_compile_error().into();
    }

    // Extract parameters, indicated by anything starting with a : in the SQL query.
    let params: Vec<String> =
        sql.split(':').skip(1).map(|s| s.split_whitespace().next().unwrap().to_string()).collect();

    // Generate the builder struct for each parameter.
    let param_struct_name = format_ident!("QueryParams");
    let param_fields = params.iter().map(|param| {
        let param_name = format_ident!("{}", param);
        quote! {
            #param_name: libsql::Value,
        }
    });

    // Generate the parameter identifier and struct value pairs for the query.
    let param_values = params.iter().map(|param| {
        let param_name = format!(":{}", param);
        let param_field = format_ident!("{}", param);
        quote! {
            #param_name: self.#param_field.clone(),
        }
    });

    let data_struct_name = format_ident!("QueryData");
    let data_fields = select_fields.iter().map(|field_info| {
        // Use this alternative to avoid name clashes.
        // let field_name = format_ident!("{}_{}", field_info.table_name, field_info.alias);
        let field_name = format_ident!("{}", field_info.alias);
        let table = table_map.get(&field_info.table_name).expect("Table not found");
        let field_type: Type = parse_str(
            table
                .columns
                .iter()
                .find(|(name, _)| name == &field_info.field_name)
                .expect("Field not found")
                .1
                .clone()
                .as_ref(),
        )
        .expect("Failed to parse field type");
        quote! {
            pub #field_name: #field_type,
        }
    });

    let expanded = quote! {
        {

            #[derive(typed_builder::TypedBuilder)]
            /// Data structure for the query parameters.
            struct #param_struct_name {
                // Unfold the struct fields for each select field.
                #(#param_fields)*
            }

            #[derive(Clone, Debug, serde::Deserialize)]
            /// Data structure for the query result.
            struct #data_struct_name {
                // Unfold the struct fields for each select field.
                #(#data_fields)*
            }

            /// Create a new instance of the builder, including our builder methods for the parameters.
            impl #param_struct_name {
                /// Run the query and return the result.
                ///
                /// NOTE: This consumes any errors from Turso, to make the API more ergonomic, since
                /// in the majority of cases, we won't do anything about the errors after all.
                pub async fn execute(&self, conn: &libsql::Connection) -> Vec<#data_struct_name> {
                    let mut stmt = conn.prepare(#sql).await.expect("Failed to set up prepared statement");
                    // let mut params: Vec<(String, libsql::Value)> = vec![];
                    // for (name, value) in &self.params {
                    //     params.push((name.clone(), value.clone()));
                    // }
                    let mut rows = stmt.query(libsql::named_params! {
                        #(#param_values)*
                    }).await.expect("Failed to execute query");
                    let mut items: Vec<#data_struct_name> = Vec::new();
                    // Get each row from the database, deserialized to `T`.
                    while let Ok(Some(row)) = rows.next().await {
                        // TODO: Deserialize the row into the struct.
                        let item = libsql::de::from_row::<#data_struct_name>(&row).expect("failed to deserialize row");
                        items.push(item);
                    }
                    items
                }

                /// Run the query and return the first result or `None` if the query returns no results.
                ///
                /// NOTE: This consumes any errors from Turso, to make the API more ergonomic, since
                /// in the majority of cases, we won't do anything about the errors after all.
                pub async fn first(&self, conn: &libsql::Connection) -> Option<#data_struct_name> {
                    let items = self.execute(conn).await;
                    items.first().cloned()
                }
            }

            // Return the builder instantiated builder.
            #param_struct_name::builder()
        }
    };

    TokenStream::from(expanded)
}

```

To test the proc-macro, we cannot set up unit tests but instead need to add an integration test. Create the director `lib-macros/tests` and add the file `lib-macros/tests/lib.rs`:

```rust ,linenos
use lib_macros::query;

#[test]
fn test_query_macro() {
    let query = query!(
        "SELECT users.user_id, users.first_name, users.last_name, users.country, users.onboarding_step, users.demo_user
        FROM auth_sessions AS sessions
        LEFT JOIN users AS users ON users.user_id = sessions.user_id
        WHERE sessions.session_id = :session"
    ).session("some-session-id".into()).build();
}
```

This will let you validate that the proc-macro actually works as expected. Adjust the test query to something that fits with your own table definition, or copy the example SQL below from the [Errors](#errors) section, and put it into the expected location in `database/users.sql`:

```rust
└── database
    └── users.sql
└── lib-macros
    └── src/...
    └── tests/...
    └── Cargo.toml
```

# Errors

Let's explore a case where we query the wrong field. We will build our example on the schema below:

```sql ,linenos
CREATE TABLE IF NOT EXISTS
  "users"
  (
    -- The user id.
    "user_id" varchar(36) NOT NULL,
    -- The user's first name.
    "first_name" varchar(255),
    -- The user's last name.
    "last_name" varchar(255),
    -- The user's country code (e.g. DK, US, etc.).
    "country" varchar(3),
    -- If the user is onboarding or not.
    -- Possible values: 'first' | 'second' | 'third' | 'done'
    "onboarding_step" varchar(255) NOT NULL DEFAULT 'first',
    -- If the user is a demo user or not.
    "demo_user" boolean NOT NULL DEFAULT false,
    -- The time the user was created.
    "created_at" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- The time the user was last updated.
    "updated_at" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (user_id)
  );

CREATE TABLE IF NOT EXISTS
  "auth_sessions"
  (
    -- The unique session id.
    "session_id" varchar(36) NOT NULL,
    -- The user id.
    "user_id" varchar(36) NOT NULL,
    -- The time the session was created.
    "created_at" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- The time the session entry was last updated.
    "updated_at" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- The time the session is set to expire.
    "expires_at" timestamp NOT NULL,
    PRIMARY KEY (session_id)
  );
CREATE INDEX IF NOT EXISTS auth_sessions_user_lookup ON auth_sessions (user_id);
```

Take the following faulty query (note the field `onboarding`, which should be `onboarding_step`):

```rust
let conn = database_context.users.get().expect("No connection to users database");
let query = query!("SELECT user_id, first_name, last_name, country, onboarding FROM users");
let query = query.build();
let rows = query.execute(&conn).await;
let users = rows
    .into_iter()
    .map(|row| User::new(row.user_id, row.first_name, row.last_name, row.onboarding_step))
    .collect();
```

It will give a compile time error:

```sql
Field 'onboarding' does not exist in table 'users'
```

Which is very helpful, since it blocks us from keeping an invalid query, and points out both the field it cannot find as well as which table it was trying to find it on.

{{ medium_comments(post="improved-turso-libsql-ergonomics-in-rust-b2c0bf134d46") }}
