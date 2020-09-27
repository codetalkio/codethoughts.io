use async_graphql::{EmptyMutation, EmptySubscription, Schema};
use lambda::Context;
use lazy_static::lazy_static;
use rusoto_core::Region;
use rusoto_dynamodb::{DynamoDb, DynamoDbClient, ListTablesInput};
use serde_json::Value;

use crate::graphql::Query;
use crate::types::*;

// We use the lazy_static crate to declaring global variables that are instantiated at runtime. This
// allows us to only pay the initialization cost once, by reling on lazily evaluated statics.
lazy_static! {
    // Set up our GraphQL schema first so we only build it once.
    static ref SCHEMA: Schema<Query, EmptyMutation, EmptySubscription> =
        Schema::new(Query, EmptyMutation, EmptySubscription);
}

pub async fn handler(event: Value, _: Context) -> Result<Value, Error> {
    let query = event["query"].as_str().unwrap_or("{}");
    let data = SCHEMA.execute(query).await?.data;
    Ok(data)
}
