import data from "./house";
let newData;

// ## Get a field.
console.log("\n\n## Get a field.");
console.log(data.house.owner);

// ## Get a nested field.
console.log("\n\n## Get a nested field.");
console.log(data.house.owner.firstname);

// ## Get an optional field.
console.log("\n\n## Get an optional field.");
// A field on an object that exists.
console.log("A field on an object that exists:", data.house.address.address);

try {
  // A field on an object that does *NOT* exist.
  console.log("A field on an object that does *NOT* exist:");
  console.log(data.house.alternativeAddress.address);
} catch (err) {
  console.error(err);
}

// A field that does *NOT* exist, using optional-chaining.
console.log(
  "A field that does *NOT* exist, using optional-chaining:",
  data.house.alternativeAddress?.address
);

// ## Update a field.
console.log("\n\n## Update a field.");
newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
const newAriel = { id: 4, firstname: "New Ariel", lastname: "Swandóttir" };
newData.house.owner = newAriel;
console.log(newData.house.owner);

// ## Update a nested field.
console.log("\n\n## Update a nested field.");
newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
newData.house.owner.firstname = "New Ariel";
console.log(newData.house.owner.firstname);

// ## Update each item in a list.
console.log("\n\n## Update each item in a list.");
newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
newData.house.people.forEach((person) => {
  person.firstname = `Fly ${person.firstname}`;
});
console.log(newData.house.people);
