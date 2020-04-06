use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Address {
    pub country: String,
    pub address: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Person {
    pub id: u32,
    pub firstname: String,
    pub lastname: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Household {
    pub id: u32,
    pub people: Vec<Person>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub address: Option<Address>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alternative_address: Option<Address>,
    pub owner: Person
}

pub fn house() -> Household {
    let addr = Address { country: "Ocean".to_string(), address: "Under the sea".to_string() };
    let mom = Person { id: 1, firstname: "Ariel".to_string(), lastname: "Swanson".to_string() };
    let dad = Person { id: 2, firstname: "Triton".to_string(), lastname: "Swanson".to_string() };
    let son = Person { id: 3, firstname: "Eric".to_string(), lastname: "Swanson".to_string() };
    Household {
        id: 1,
        people: vec![mom.clone(), dad, son],
        address: Some(addr),
        alternative_address: None,
        owner: mom
    }
}
