---
title: "The Stack Part 4: A Federated GraphQL API"
tags: rust, aws, cloud, infrastructure, cdk
---

In [the last post](/posts/2023-10-07-the-stack-part-2.html) we built up two Frontend Apps, one using Next.js and another using Leptos (Rust/WASM). In this post we will be building an API that our Apps can talk to. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

At the end of this post we will have:

- A Federated GraphQL API using [Apollo Federation v2](https://www.apollographql.com/docs/federation/).
- A supergraph, using [Apollo Router](https://www.apollographql.com/docs/router/), composing our services into a single GraphQL API.
- Three small [Rust-based GraphQL](https://github.com/async-graphql/async-graphql) Subgraphs.
- Automatic deployment of our services to AWS Lambda.

There is quite a lot to cover. My recommendation is to clone down the Part 4 branch in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-4-api) and use this post as an explanation of what is set up.

<div></div><!--more-->

- [Prelude: Federated GraphQL](#prelude-federated-graphql)
- [Apollo Router](#apollo-router)
- [Subgraph: Products](#subgraph-products)
- [Subgraph: Users](#subgraph-users)
- [Subgraph: Reviews](#subgraph-reviews)
- [Next Steps](#next-steps)


## Prelude: Federated GraphQL

If you are new to schema federation, then I recommend reading the [Apollo Federation docs](https://www.apollographql.com/docs/federation/). In short, schema federation is a way to compose multiple GraphQL schemas into a single schema. This is done by defining a `@key` directive on types in each schema, which tells the gateway how to resolve references to that type (very simplified).

The problem federation solves is one of growth—both our codebase as well as our organization.

As we grow our API and our domains, they will start to become hard to keep separate in a traditional monolith architecture. We don't build on a foundation that requires good discipline, we build discipline into our constructs and foundation.

> Discipline doesn't scale
>
> _- Probably someone, somewhere_

Instead we want to design for a future where multiple teams will work on our services, while keeping the overhead low so that it does not slow us down while we are still small.

Our initial architecture will look like this:

<div style="text-align:center;">
<pre class="mermaid">
graph LR
  Client --> Router[λ Apollo Router]
  subgraph Supergraph
    Router --> Products[λ Products\n subgraph]
    Router --> Users[λ Users\n subgraph]
    Router --> Reviews[λ Reviews\n subgraph]
  end
  style Supergraph stroke:#333,stroke-width:2px,fill:transparent
</pre>
</div>

Each of these services, included the Apollo Router itself, will be deployed as AWS Lambda functions. The Apollo Router is responsible for knowing how to compose each of the subgraphs, and how to resolve references between them.

## Our GraphQL Schema

We will base our services on the example subgraphs from [Apollo's intro to Federation](https://www.apollographql.com/docs/federation/#concern-based-separation).

**Our Users schema:**

```graphql
type Query {
  # Get the currently logged in user.
  me: User

  # List all users.
  users: [User!]!
}

# Users have a name and nothing more.
type User @key(fields: "id") {
  id: ID!
  name: String!
}
```

**Our Products schema:**

```graphql
type Query {
  # Retrive a specific product by ID.
  product(id: ID!): Product!

  # Retrive a list of all products.
  products: [Product!]!
}

# Products simply have a name and price.
type Product @key(fields: "id") {
  id: ID!
  name: String!
  price: String!
}

# External Type: User.
type User @key(fields: "id") {
  id: ID!
  # Extend the Users type with a purchases field.
  purchases: [Product!]!
}
```

**Our Reviews schema:**

```graphql
type Query {
  # Retrive a specific review by ID.
  review(id: ID!): Review!

  # Retrive a list of all reviews.
  reviews: [Review!]!
}

# Reviews are written by a User and are about a Product.
type Review @key(fields: "id") {
  id: ID!
  body: String!
  author: User!
  product: Product!
}

# External Type: User.
type User @key(fields: "id") {
  id: ID!
  # Extend the Users type with a reviews field.
  reviews: [Review!]!
}

# External Type: Product.
type Product @key(fields: "id") {
  id: ID!
  # Extend the Product type with a reviews field.
  reviews: [Review!]!
}
```

All schemas extend the schema, which you may or may not need to do depending on your GraphQL tooling/server:

```graphql
extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.0", import: ["@key"])
```

## Subgraph: Products, Users, and Reviews

We will start from our building blocks, which will be our subgraphs, and then build up to the Router that will compose these into the supergraph.



The Router needs an HTTP endpoint, we will use [AWS Lambda Function URLs](https://aws.amazon.com/blogs/aws/announcing-aws-lambda-function-urls-built-in-https-endpoints-for-single-function-microservices/).


Price per 1ms (eu-west-1):
- 128MB = $0.0000000017
- 512MB = $0.0000000067
- 1024MB = $0.0000000133
- 1536MB = $0.0000000200
- 2048MB = $0.0000000267

Cold-start:
- ms-gql-reviews
  - Memory: 128MB
  - Billed Duration: 67ms
  - Total time: 257ms
  - Init:
  - Invocation:
- ms-gql-users
  - Memory: 128MB
  - Billed Duration: 57ms
  - Total time: 210ms
  - Init:
  - Invocation:
- ms-gql-products
  - Memory: 128MB
  - Billed Duration: 53ms
  - Total time: 164ms
  - Init:
  - Invocation:

Warm-start:
- ms-gql-reviews
  - Memory: 128MB
  - Billed Duration: 2ms
  - Total time: 7ms
- ms-gql-users
  - Memory: 128MB
  - Billed Duration: 1ms
  - Total time: 6ms
- ms-gql-products
  - Memory: 128MB
  - Billed Duration: 2ms
  - Total time: 6ms


Cold-start:
- ms-gql-reviews
  - Memory: 1024MB
  - Billed Duration: 45ms
  - Total time: 226ms
  - Init: 39ms
  - Invocation: 1ms
- ms-gql-users
  - Memory: 1024MB
  - Billed Duration: 44ms
  - Total time: 201ms
  - Init: 38ms
  - Invocation: 4ms
- ms-gql-products
  - Memory: 1024MB
  - Billed Duration: 43ms
  - Total time: 202ms
  - Init: 39ms
  - Invocation: 1ms

Warm-start:
- ms-gql-reviews
  - Memory: 1024MB
  - Billed Duration: 2ms
  - Total time: 6ms
- ms-gql-users
  - Memory: 1024MB
  - Billed Duration: 2ms
  - Total time: 8ms
- ms-gql-products
  - Memory: 1024MB
  - Billed Duration: 2ms
  - Total time: 6ms

## DynamoDB: A database for our services

## Apollo Router

## Automating Deployments via CDK

#### Building artifacts in CI


#### Deploying to AWS Lambda

## H2 Headline

### H3 Headline

#### H4 Headline

##### H5 Headline

###### H6 Headline


## Next Steps

Next up is to use our new API from our existing Frontend Apps! Follow along in Part 5 of the series (will be posted soon).
