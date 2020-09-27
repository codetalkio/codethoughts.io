use lambda::Context;
use serde_json::json;
use sls_rust::handler::handler;

#[tokio::test]
async fn handler_handles_basic_event() {
    let event = json!({
        "query": "{ add(a: 22, b: 20) }"
    });
    let result = json!({
        "add": 42
    });
    assert_eq!(
        handler(event.clone(), Context::default())
            .await
            .expect("expected Ok(_) value"),
        result
    )
}
