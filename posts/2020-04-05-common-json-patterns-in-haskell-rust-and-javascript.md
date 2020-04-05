---
title: Common JSON patterns in Haskell, Rust and TypeScript
tags: haskell, lens, javascript, typescript, rust, records, serde, aeson
---

A lot of web development is transforming JSON one way or another. In TypeScript/JavaScript this is straightforward, since JSON is built into the language. But can we also achieve good ergonomics in Haskell and Rust? *Dear reader, I am glad you asked! 🙌*

The comparisons we will see is not meant to show if one approach is better than another. Instead, it is intended to be a reference to become familiar with common patterns across multiple languages. Throughout this post we will utilize several tools and libraries.

The core of working with JSON in Haskell and Rust is covered by:

- [Aeson](https://hackage.haskell.org/package/aeson): a Haskell JSON serialization/deserialization library[^derivingAeson].
- [Serde](https://serde.rs): a Rust JSON serialization/deserialization library.

The ergonomics is then improved in Haskell by grabbing one of the following options[^moreOptions]:

- [Lens](https://hackage.haskell.org/package/lens): a heavy-weight library to transform and work with records (and much more!)[^genericLens].
- [Record Dot Syntax](https://github.com/shayne-fletcher-da/ghc-proposals/blob/record-dot-syntax/proposals/0000-record-dot-syntax.md): an upcoming language extension in Haskell, which [recently got accepted](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-608329102) by the GHC steering Committee[^recordDotSyntax].

We'll go through typical use-cases you will see in a lot of TypeScript/JavaScript codebases, and see how we can achieve the same in Haskell and Rust.

<div></div><!--more-->

**Table of Contents:**

- [Preparation: Setting up our data](#preparation-setting-up-our-data)
- [Comparison](#comparison)
    - [Get a field](#get-a-field)
    - [Get a nested field](#get-a-nested-field)
    - [Get an optional field](#get-an-optional-field)
    - [Update a field](#update-a-field)
    - [Update a nested field](#update-a-nested-field)
    - [Update each item in a list](#update-each-item-in-a-list)


## Preparation: Setting up our data

First we will setup our data structures and a few examples, which we will use throughout this post. Haskell and Rust requires a bit more ceremony, because we will use packages/crates. For TypeScript we use `ts-node` to run TypeScript in a REPL.

**TypeScript**

Let us first set up our reference Object in TypeScript. Save the following in `house.ts` (or checkout [typescript-json](https://github.com/codetalkio/codetalk.io/tree/master/resources/code/common-json-patterns/typescript-json)):

```typescript
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
```

**Haskell**

The included snippet serves to give you an idea of the datastructures, types, and names that we will be working with.

The setups for each specific solution can be found in:

- [haskell-lens](https://github.com/codetalkio/codetalk.io/tree/master/resources/code/common-json-patterns/haskell-lens): Contains the Lens apporach.
- [haskell-record-dot](https://github.com/codetalkio/codetalk.io/tree/master/resources/code/common-json-patterns/haskell-record-dot): Contains the Record Dot Syntax apporach.

Check out `src/House.hs` for the data structures, and `src/Main.hs` for all the examples throughout this post.


```haskell
data Address = Address
  { country :: String
  , address :: String
  }

data Person = Person
  { id :: Int
  , firstname :: String
  , lastname :: String
  }

data Household = Household
  { id :: Int
  , people :: [Person]
  , address :: Maybe Address
  , alternativeAddress :: Maybe Address
  , owner :: Person
  }

house = Household
  { id = 1
  , people = [mom, dad, son]
  , address = Just addr
  , alternativeAddress = Nothing
  , owner = mom
  }
  where
    addr = Address { country = "Ocean", address = "Under the sea" }
    mom = Person { id = 1, firstname = "Ariel", lastname = "Swanson" }
    dad = Person { id = 2, firstname = "Triton", lastname = "Swanson" }
    son = Person { id = 3, firstname = "Eric", lastname = "Swanson" }
```

To allow overlapping record fields, we use [DuplicateRecordFields](https://github.com/adamgundry/ghc-proposals/blob/overloaded-record-fields/proposals/0000-overloaded-record-fields.rst#recap-duplicaterecordfields) along with [OverloadedLabels](https://github.com/adamgundry/ghc-proposals/blob/overloaded-record-fields/proposals/0000-overloaded-record-fields.rst#recap-overloadedlabels) (only in the Lens version), and a bunch of other extensions for deriving things via generics.

**Rust**

The full setup can be found in [rust-serde](https://github.com/codetalkio/codetalk.io/tree/master/resources/code/common-json-patterns/rust-serde). Check out `src/house.rs` for the data structures, and `src/main.rs` for all the examples throughout this post.

```rust
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
        owner: mom.clone()
    }
}
```


## Comparison

If you wish to following along and play with the approaches, you can fire up a REPL for each approach:

**TypeScript**
```bash
$ cd typescript-json
$ npm i
$ npm run repl
> import data from './house';
```

**Haskell**
```bash
$ cd haskell-lens
$ stack build
$ stack ghci
*Main Data>
```


Unfortunately GHC plugins doesn't play nicely with `ghci`, so we will instead build the project, and then you can play around with the examples in `src/Main.hs`.

```bash
$ cd haskell-record-dot
$ stack build
$ # Open src/Main.hs in your editor
$ stack run
```

**Rust**

Since Rust doesn't have a REPL, we will instead build the project, and then you can play around with the examples in `src/main.rs`.

```bash
$ cd rust-serde
$ cargo build
$ # Open src/main.rs in your editor
$ cargo run
```

### Get a field
The first one is simple, we'll simply get a value in the object.

First, our **TypeScript** version:
```typescript
> data.house.owner
{ id: 1, firstname: 'Ariel', lastname: 'Swanson' }
```

Let's see how we achieve this in **Haskell with Lenses:**
```haskell
*Main Data> house ^. #owner
Person {id = 1, firstname = "Ariel", lastname = "Swanson"}
```

There's probably already two unfamiliar pieces of syntax here.

The first, `^.`, comes from Lens and is the `view` function that we use as an accessor to the object/record. The second, the `#` prefix of `#owner`, comes from the `OverloadedLabels` extension, and allows us to have multiple record fields of the same name in scope.

Let's see how we achieve this in **Haskell with Record Dot Syntax:**
```haskell
house.owner
--> Person {id = 1, firstname = "Ariel", lastname = "Swanson"}
```

Finally, let's check out **Rust:**
```rust
house.owner
--> Person { id: 1, firstname: "Ariel", lastname: "Swanson" }
```

### Get a nested field
We slowly increase the difficulty, by accessing a nested field.

**TypeScript:**
```typescript
> data.house.owner.firstname
'Ariel'
```

**Haskell with Lenses:**
```haskell
*Main Data> house ^. #owner . #firstname
"Ariel"
```

**Haskell with Record Dot Syntax:**
```haskell
house.owner.firstname
--> "Ariel"
```

**Rust:**
```rust
house.owner.firstname
--> "Ariel"
```

### Get an optional field
How do we handle optional fields?

**TypeScript:**
```typescript
// A field that exists.
> data.house.address.address
'Under the sea'

// A field that does *NOT* exist (throws exception.)
> data.house.alternativeAddress.address
TypeError: Cannot read property 'address' of undefined
    at ....

// A field that does *NOT* exist, using optional-chaining.
> data.house.alternativeAddress?.address
undefined
```

Optional chaining (`?`) is a great step forward for writing safer and cleaner code in JS/TS.

**Haskell with Lenses:**
```haskell
-- Return the value in a Maybe.
*Main Data> house ^. #address
Just (Address {country = "Ocean", address = "Under the sea"})

-- A field on an object that exists.
*Main Data> house ^. #address . #_Just . #address
"Under the sea"

-- A field on an object that does *NOT* exist (falls back to an empty value.)
*Main Data> house ^. #alternativeAddress . #_Just . #address
""
```

`#_Just` from Lens gives us handy access to fields wrapped in `Maybe`s, with a fallback value.

**Haskell with Record Dot Syntax:**
```haskell
-- Return the value in a Maybe.
house.address
--> Just (Address {country = "Ocean", address = "Under the sea"})

-- A field on an object that exists.
maybe "" (.address) house.address
--> "Under the sea"

-- A field on an object that does *NOT* exist (falls back to an empty value.)
maybe "" (.address) house.alternativeAddress
--> ""
```

We end up writing more regular code to dive into the `Maybe` value, by using `maybe`[^dataMaybe] to proceed or fallback to a default value.

**Rust:**
```rust
// Return the value in an Option.
house.address
--> Some(Address { country: "Ocean", address: "Under the sea" })

// A field on an object that exists.
house.address.and_then(|a| Some(a.address)).unwrap_or("".to_string()
--> "Under the sea"

// A field on an object that does *NOT* exist (falls back to an empty value.)
house.alternative_address.and_then(|a| Some(a.address)).unwrap_or("".to_string())
--> ""
```
We utilize `and_then` a bit like `maybe`, passing a function to act on our value if it's `Some`, and then creating a default case with `unwrap_or`.


### Update a field
We'll start with updating a non-nested field. Note that for the JavaScript versions we will clone our objects before doing any mutations, to keep our `data` object consistent througout the examples.

**TypeScript:**
```typescript
> newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
> const newAriel = { id: 4, firstname: 'New Ariel', lastname: 'Swandóttir' }
> newData.house.owner = newAriel
{ id: 4, firstname: 'New Ariel', lastname: 'Swandóttir' }
```

**Haskell with Lenses:**
```haskell
*Main Data> let newAriel = Person { id = 4, firstname = "New Ariel", lastname = "Swanson" }
*Main Data> house & #owner .~ newAriel
Household { {- Full Household object... -} }
```

We add two pieces of syntax here. The `&` is a reverse application operator, but for all intents and purposes, think of it as the `^.` for setters. Finally, `.~` is what allows us to actually set our value.

**Haskell with Record Dot Syntax:**
```haskell
let newAriel = Person { id = 4, firstname = "New Ariel", lastname = "Swanson" }
house{owner = newAriel}
--> Household { {- Full Household object... -} }
```

**Rust:**
```rust
let new_ariel = Person { id: 4, firstname: "New Ariel".to_string(), lastname: "Swanson".to_string() };
Household { owner: new_ariel, ..house }
--> Household { /* Full Household object... */ }
```

We use Rust's [Struct Update syntax](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax), `..`, which works much like the spread syntax (`...`) in JavaScript.

### Update a nested field
Now it gets a bit more tricky.

**TypeScript:**
```typescript
> newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
> newData.house.owner.firstname = 'New Ariel'
'New Ariel'
```

**Haskell with Lenses:**
```haskell
*Main Data> house & #owner . #firstname .~ "New Ariel"
Household { {- Full Household object... -} }
```

Note that we mix `&` and `.` to dig deeper in the object/record, much like accessing a nested field.

**Haskell with Record Dot Syntax:**
```haskell
house{owner.firstname = "New Ariel"}
--> Household { {- Full Household object... -} }
```

Note that the lack of spacing in `house{owner.firstname` is actually important, at least in the current state of [RecordDotSyntax](https://github.com/shayne-fletcher-da/ghc-proposals/blob/record-dot-syntax/proposals/0000-record-dot-syntax.md#3-examples).

**Rust:**
```rust
Household { owner: Person { firstname: "New Ariel".to_string(), ..house.owner }, ..house }
--> Household { /* Full Household object... */ }
```

### Update each item in a list
Let's work a bit on the people list in our household. We'll make those first names a bit more fresh.

**TypeScript:**
```typescript
> newData = JSON.parse(JSON.stringify(data)); // Clone our data object.
> newData.house.people.map(person => { person.firstname = `Fly ${person.firstname}` })
> newData.house.people
[
  { id: 1, firstname: 'Fly Ariel', lastname: 'Swanson' },
  { id: 2, firstname: 'Fly Triton', lastname: 'Swanson' },
  { id: 3, firstname: 'Fly Eric', lastname: 'Swanson' }
]
```

**Haskell with Lenses:**
```haskell
*Main Data> house & #people . mapped %~ (\p -> p & #firstname .~ "Fly " ++ p ^. #firstname)
Household { {- Full Household object... -} }
```

`mapped` allows us to map a function over all the values in `#people`.

**Haskell with Record Dot Syntax:**
```haskell
house{people = map (\p -> p{firstname = "Fly " ++ p.firstname}) house.people}
--> Household { {- Full Household object... -} }
```

Using `map` feels very natural, and is quite close to the regular code you would write in Haskell.

**Rust:**
```rust
Household {
    people: house.people.into_iter().map(|p| {
        Person { firstname: format!("Fly {}", p.firstname).to_string(), ..p }
    }).rev().collect(),
    ..house
}
--> Household { /* Full Household object... */ }
```

<br />

---

Have other common patterns you'd like to see? Feel like some of the approaches could be improved? Leave a comment, and I will try to expand this list to be more comprehensive!

[^moreOptions]: There are of course more options, like [Optics](https://www.well-typed.com/blog/2019/09/announcing-the-optics-library/) ([usage example](https://www.reddit.com/r/haskell/comments/cyo4o2/welltyped_announcing_the_optics_library/eywc9ya?utm_source=share&utm_medium=web2x)), but I won't cover them all here.

[^derivingAeson]: Along with [aeson](https://hackage.haskell.org/package/aeson), we will use the new [deriving-aeson](https://hackage.haskell.org/package/deriving-aeson) library to derive our instances.

[^genericLens]: We use [generic-lens](https://github.com/kcsongor/generic-lens) for Lens derivations instead of TemplateHaskell.

[^recordDotSyntax]: It will take a bit of time before it is merged and available in GHC, so we will use the [record-dot-preprocessor](https://hackage.haskell.org/package/record-dot-preprocessor) plugin to get a sneak peak.

[^dataMaybe]: `maybe` from Data.Maybe has the type signature `maybe :: b -> (a -> b) -> Maybe a -> b`, taking in as argument (1) a default value (2) a function to run if the value is `Just` and (3) the `Maybe` value we want to operate on.
