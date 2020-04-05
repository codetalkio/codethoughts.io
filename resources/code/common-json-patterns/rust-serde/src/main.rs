mod house;

use house::*;

fn main() {
    {
        let house = house();

        // Showcase our derived JSON object.
        println!("\n\nShowcase our derived JSON object:");
        let serialized = serde_json::to_string(&house).unwrap();
        println!("{}", serialized);

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
        println!(" {:?}", house.address.and_then(|a| Some(a.address)).unwrap_or("".to_string()));

        println!("\nA field on an object that does *NOT* exist (falls back to an empty value.)");
        println!("{:?}", house.alternative_address.and_then(|a| Some(a.address)).unwrap_or("".to_string()));
    }

    // ## Update a field.
    {
        println!("\n\n## Update a field.");
        let house = house();
        let new_ariel = Person { id: 4, firstname: "New Ariel".to_string(), lastname: "Swanson".to_string() };
        println!("{:?}", Household { owner: new_ariel, ..house });
    }

    // ## Update a nested field.
    {
        println!("\n\n## Update a nested field.");
        let house = house();
        println!("{:?}", Household { owner: Person { firstname: "New Ariel".to_string(), ..house.owner }, ..house });
    }

    // ## Update each item in a list.
    {
        println!("\n\n## Update each item in a list.");
        let house = house();
        println!("{:?}", Household {
            people: house.people.into_iter().map(|p| {
                Person { firstname: format!("Fly {}", p.firstname).to_string(), ..p }
            }).rev().collect(),
            ..house
        });
    }

}
