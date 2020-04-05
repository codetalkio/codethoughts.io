use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Address<'a> {
    pub country: &'a str,
    pub address: &'a str,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub struct Person<'a> {
    pub id: u32,
    pub firstname: &'a str,
    pub lastname: &'a str,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Household<'a> {
    pub id: u32,

    #[serde(borrow)]
    pub people: Vec<Person<'a>>,

    #[serde(borrow)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub address: Option<Address<'a>>,

    #[serde(borrow)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alternative_address: Option<Address<'a>>,

    #[serde(borrow)]
    pub owner: Person<'a>
}

pub fn house<'a>() -> Household<'a> {
    let addr = Address { country: "Ocean", address: "Under the sea" };
    let mom = Person { id: 1, firstname: "Ariel", lastname: "Swanson" };
    let dad = Person { id: 2, firstname: "Triton", lastname: "Swanson" };
    let son = Person { id: 3, firstname: "Eric", lastname: "Swanson" };
    Household {
        id: 1,
        people: vec![mom, dad, son],
        address: Some(addr),
        alternative_address: None,
        owner: mom
    }
}
