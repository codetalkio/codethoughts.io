use async_graphql::Object;

// Our GraphQL Schema.
pub struct Query;

#[Object]
impl Query {
    #[field(desc = "Returns the sum of a and b")]
    async fn add(&self, a: i32, b: i32) -> i32 {
        a + b
    }
}
