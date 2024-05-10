interface Address {
  country: string;
  address: string;
}

interface Person {
  id: number;
  firstname: string;
  lastname: string;
}

interface Household {
  id: number;
  people: Person[];
  address?: Address;
  alternativeAddress?: Address;
  owner: Person;
}

const addr: Address = { country: "Ocean", address: "Under the sea" };
const mom: Person = { id: 1, firstname: "Ariel", lastname: "Swanson" };
const dad: Person = { id: 2, firstname: "Triton", lastname: "Swanson" };
const son: Person = { id: 3, firstname: "Eric", lastname: "Swanson" };
const house: Household = {
  id: 1,
  people: [mom, dad, son],
  address: addr,
  // We omit `alternativeAddress` which is optional.
  owner: mom,
};

export default { mom, dad, son, house };
