use async_graphql::{EmptyMutation, EmptySubscription, Schema};
use lazy_static::lazy_static;
use rusoto_core::Region;
use rusoto_dynamodb::{DynamoDb, DynamoDbClient, ListTablesInput};
use serde_json::Value;

use crate::graphql::Query;

// We use the lazy_static crate to declaring global variables that are instantiated at runtime. This
// allows us to only pay the initialization cost once, by reling on lazily evaluated statics.
lazy_static! {
    /// Set up our GraphQL schema first so we only build it once.
    pub static ref SCHEMA: Schema<Query, EmptyMutation, EmptySubscription> =
        Schema::new(Query, EmptyMutation, EmptySubscription);

    /// Set up our DynamoDB client to avoid needing to reconnect.
    /// FIXME: Retrieve region from environment variables.
    pub static ref DYNAMODB_CLIENT: DynamoDbClient = DynamoDbClient::new(Region::EuCentral1);
}
