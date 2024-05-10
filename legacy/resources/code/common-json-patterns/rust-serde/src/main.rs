mod house;

use house::*;

fn main() {
    let house = house();
    {
        let house = house.clone();
        // ## Get a field.
        println!("\n\n## Get a field.");
        println!("{:?}", house.owner);

        // ## Get a nested field.
        println!("\n\n## Get a nested field.");
        println!("{:?}", house.owner.firstname);

        // ## Get an optional field.
        println!("\n\n## Get an optional field.");
        println!("Return the value in an Option:");
        println!("{:?}", house.address);

        println!("\nA field on an object that exists:");
        println!(
            " {:?}",
            house
                .address
                .and_then(|a| Some(a.address))
                .unwrap_or("".to_string())
        );

        println!("\nA field on an object that does *NOT* exist (falls back to an empty value.)");
        println!(
            "{:?}",
            house
                .alternative_address
                .and_then(|a| Some(a.address))
                .unwrap_or("".to_string())
        );
    }

    // ## Update a field.
    {
        println!("\n\n## Update a field.");
        let mut new_house = house.clone();
        let new_ariel = Person {
            id: 4,
            firstname: "New Ariel".to_string(),
            lastname: "Swanson".to_string(),
        };
        new_house.owner = new_ariel;
        println!("{:?}", new_house);
    }

    // ## Update a nested field.
    {
        println!("\n\n## Update a nested field.");
        let mut new_house = house.clone();
        new_house.owner.firstname = "New Ariel".to_string();
        println!("{:?}", new_house);
    }

    // ## Update each item in a list.
    {
        println!("\n\n## Update each item in a list.");
        let mut new_house = house.clone();
        new_house
            .people
            .iter_mut()
            .for_each(|p| p.firstname = format!("Fly {}", p.firstname));
        println!("{:?}", new_house);
    }

    // ## Encode / Serialize.
    {
        println!("\n\n## Encode / Serialize:");
        let serialized = serde_json::to_string(&house).unwrap();
        println!("{}", serialized);
    }

    // ## Decode / Deserialize.
    {
        println!("\n\n## Decode / Deserialize:");
        let house_json = serde_json::to_string(&house).unwrap();
        let deserialize: Household = serde_json::from_str(&house_json).unwrap();
        println!("{:?}", &deserialize);
    }
}
