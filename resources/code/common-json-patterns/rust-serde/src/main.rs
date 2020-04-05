mod house;

use house::house;

fn main() {
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

    println!("A field on an object that exists:");
    println!(" {:?}", house.address.and_then(|a| Some(a.address)).unwrap_or(""));

    println!("A field on an object that does *NOT* exist (falls back to an empty value.)");
    println!("{:?}", house.alternative_address.and_then(|a| Some(a.address)).unwrap_or(""));

    // ## Update a field.


    // ## Update a nested field.


    // ## Update each item in a list.

}
