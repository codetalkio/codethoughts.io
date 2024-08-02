+++
title = "The Stack Part 4: A Federated GraphQL API"
date = 2024-03-10

[taxonomies]
tags = ["rust", "aws", "cloud", "infrastructure", "cdk"]

[extra]
mermaidjs = "true"
+++

In [the last post](@/posts/2023-10-08-the-stack-part-2/index.md) we built up two Frontend Apps, one using Next.js and another using Leptos (Rust/WASM). In this post we will be building an API that our Apps can talk to. See the full overview of posts [here](@/posts/2023-01-29-the-stack/index.md#what-will-we-be-covering).

At the end of this post we will have:

- A Federated GraphQL API using [Apollo Federation v2](https://www.apollographql.com/docs/federation/).
- A supergraph, using [Apollo Router](https://www.apollographql.com/docs/router/), composing our services into a single GraphQL API.
- Three small [Rust-based GraphQL](https://github.com/async-graphql/async-graphql) Subgraphs.
- Automatic deployment of our services to AWS Lambda.

There is quite a lot to cover. My recommendation is to clone down the Part 4 branch in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-4-api) and use this post as an explanation of what is set up.

<div></div><!-- more -->

{{ toc() }}


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


### Local Development

lib-handler

### Rust GraphQL Server

### Performance & Pricing

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

## Apollo Router

A rabbit hole/detour [apollo-router-lambda](https://github.com/codetalkio/apollo-router-lambda).


### Performance & Pricing: App Runner v.s. Lambda

Compare Apollo Gateway v.s. App Runner.


## Automating Deployments via CDK

### Configurable environments

We have multiple environments, and multiple ways we can deploy and orchestrate our services. Instead of enforcing every deployment to make the same tradeoff between cost and performance, we want to make it configurable so that we can choose the right tradeoff for each environment.

For example:

- Optimize for cost: `Developer`, `Preview`, and `Integration Test`
- Optimize for performance: `Production Multi-tenant` and `Production Single-tenant`

To help facilitate this we will create a few helper functions and types that allow us to construct our configurations safely.

Let's first get the types out of the way, create a new file `deployment/lib/types.ts`:

```typescript
/**
 * Make the possible environments available during runtime by constructing them
 * as a const array.
 */
export const validEnvironments = [
  'Developer',
  'Preview',
  'Integration Test',
  'Production Single-tenant',
  'Production Multi-tenant',
] as const;

/**
 * The possible environments as a type, inferred from `validEnvironments`.
 */
type Environment = (typeof validEnvironments)[number];

/**
 * Mapping between environment and configuration. The `Base` configuration is required, but
 * the rest are optional and will fall back to `Base` if not specified.
 */
export type ConfigMap = { Base: Config } & { [key in Environment]?: Config };

export type Config = {
  /**
   * Which Applications to deploy:
   * - internal: Leptos (Rust/WASM)
   * - app: React/Next.js (TypeScript)
   *
   * NOTE: Only `internal` supports specifying a subdomain. The `app` is
   * always located at the root.
   *
   * Example:
   * ```ts
   * apps: [
   *   { service: "internal", subdomain: "internal" },
   *   { service: "app" }
   * ]
   * ```
   */
  apps: App[];

  /**
   * Which supergraph to use:
   * - router (app-runner): [Apollo Router](https://www.apollographql.com/docs/router/)
   * - router (lambda): [Custom Apollo Router Lambda](https://github.com/codetalkio/apollo-router-lambda)
   * - mesh: [GraphQL Mesh](https://the-guild.dev/graphql/mesh)
   * - gateway: [Apollo Gateway](https://www.apollographql.com/docs/apollo-server/using-federation/apollo-gateway-setup)
   *
   * The `path` defines the path on the App domains where the API will be
   * accessible, to avoid running into CORS issues by needing to go cross-domain.
   *
   * The `pinToVersionedApi` flag will make the Apps use a pinned version of the Supergraph,
   * which prevents client<->api drift, but requires a new deployment of the App CloudFront distribution
   * to pick up the new version.
   *
   * Example:
   * ```ts
   * supergraph: {
   *   service: "mesh",
   *   runtime: "lambda",
   *   path: "/graphql",
   * }
   * ```
   */
  supergraph: Supergraph;

  /**
   * Specify the set of subgraphs to run.
   *
   * NOTE: The `name` is used to identify the subgraph in the supergraph, and
   * will be used to construct the environment variable with its URL that is
   * passed to the supergraph. E.g. a name of `users` will turn into `SUBGRAPH_USERS_URL`.
   *
   * Example:
   * ```ts
   * subgraphs: [
   *   { name: "users", project: "ms-gql-users" },
   *   { name: "products", project: "ms-gql-products" },
   *   { name: "reviews", project: "ms-gql-reviews" },
   * ]
   * ```
   */
  subgraphs: Subgraph[];

  /**
   * Experimental features.
   */
  experimental?: {
    /**
     * Set up additional supergraphs.
     *
     * NOTE: Make sure to not overlap the paths.
     */
    additionalSupergraphs?: Supergraph[];
  };
};

export type Subgraph = {
  name: string;
  project: string;
  runtime?: 'lambda';
  memory?: 128 | 256 | 512 | 1024 | 2048 | 3072 | 4096 | 5120 | 6144 | 7168 | 8192 | 9216 | 10240;
};

export type App = { service: 'internal'; subdomain: string } | { service: 'app' };

export type Supergraph =
  | {
      service: 'mesh';
      runtime: 'lambda';
      path: string;
      pinToVersionedApi: boolean;
    }
  | {
      service: 'gateway';
      runtime: 'lambda';
      path: string;
      pinToVersionedApi: boolean;
    }
  | {
      service: 'router';
      runtime: 'lambda';
      path: string;
      pinToVersionedApi: boolean;
    }
  | {
      service: 'router';
      runtime: 'app-runner';
      path: string;
    };
```

There's a lot going on, but all you need to care about is that we very specifically make sure only a valid configuration can be constructed.



Finally, we'll add some helpers to make it nicer to work with the configuration within our deployment files. Create a new file `deployment/lib/helpers.ts`:

```typescript
import { config as configMap } from '../config';
import { Config, Supergraph, App, validEnvironments } from './types';

/**
 * Resolve the configuration for the current environment and fall back to
 * the `Base` environment if no explicit configuration is found.
 */
const resolveConfig = (env: string | undefined): Config => {
  if (!env) {
    throw new Error('ENVIRONMENT not set');
  } else if (!validEnvironments.includes(env as any)) {
    throw new Error(`ENVIRONMENT '${env}' is not a valid option. Possible values ${validEnvironments.join(', ')}`);
  } else if (env in configMap) {
    return configMap[env];
  }
  return configMap['Base'];
};

/**
 * The configuration for the current environment.
 */
export const config: Config = resolveConfig(process.env.ENVIRONMENT);

/**
 * Construct a type that becomes concrete based on which record is passed in. This
 * utilizes discriminated unions so that we can make the input type of `stackFn` dependent
 * on the input of e.g. `name` and/or `runtime` in the `setupSupergraph`/`setupApp` functions.
 *
 * Example:
 * ```ts
 * const setupApp = <N extends App['service']>(name: N, stackFn: (appConfig: Specific<App, { service: N }>) => void) => {
 *   // ..
 * }
 * ```
 */
type Specific<S, R> = Extract<S, R>;

/**
 * Convenience function for looking up relevant Supergraph configurations and setting
 * up a supergraph along with its routes.
 *
 * Example:
 * ```ts
 * setupSupergraph('router', 'lambda', supergraphRoutes, (config) => {
 *   const supergraph = new lambdaFn.Stack(this, 'MsRouterLambda', {
 *     ...props,
 *     functionName: 'ms-router',
 *     assets: 'artifacts/ms-router',
 *     billingGroup: 'ms-router',
 *     architecture: lambda.Architecture.X86_64,
 *     environment: {
 *       ...subGraphUrls,
 *     },
 *   });
 *   // ..
 *   return config?.pinToVersionedApi ? supergraph.aliasUrlParameterName : supergraph.latestUrlParameterName;
 * });
 * ```
 */
export const setupSupergraph = <N extends Supergraph['service'], R extends Supergraph['runtime']>(
  name: N,
  runtime: R,
  supergraphRoutes: { [key: string]: string },
  stackFn: (additionalConfig?: Specific<Supergraph, { service: N; runtime: R }>) => string,
) => {
  const isMainSupergraph = config.supergraph.service === name && config.supergraph.runtime === runtime;
  // We cast our result to `undefined | Specific` to narrow down the type.
  const additionalSupergraphConfig = config.experimental?.additionalSupergraphs?.find(
    (s) => s.service === name && s.runtime === runtime,
  ) as undefined | Specific<Supergraph, { service: N; runtime: R }>;

  // If the supergraph is the main one, or if it's an additional supergraph, set up the stack.
  if (isMainSupergraph || additionalSupergraphConfig) {
    const url = stackFn(additionalSupergraphConfig);
    if (isMainSupergraph) {
      supergraphRoutes[config.supergraph.path] = url;
    }
    if (additionalSupergraphConfig) {
      supergraphRoutes[additionalSupergraphConfig.path] = url;
    }
  }
};

/**
 * Convenience function for looking up relevant App configurations and setting
 * up the App stack.
 *
 * Example:
 * ```ts
 * setupApp('internal', (appConfig) => {
 *   new s3Website.Stack(this, 'WebsiteUiInternal', {
 *     ...props,
 *     assets: 'artifacts/ui-internal',
 *     index: 'index.html',
 *     error: 'index.html',
 *     domain: `${appConfig.subdomain}.${props.domain}`,
 *     hostedZone: props.domain,
 *     certificateArn: props.certificateArn,
 *     billingGroup: 'ui-internal',
 *     redirectPathToUrl: supergraphRoutes,
 *   });
 * });
 * ```
 */
export const setupApp = <N extends App['service']>(
  name: N,
  stackFn: (appConfig: Specific<App, { service: N }>) => void,
) => {
  // We cast our result to `undefined | Specific` to narrow down the type.
  const appConfig = config.apps.find((app) => app.service === name) as undefined | Specific<App, { service: N }>;

  if (appConfig && appConfig.service === name) {
    return stackFn(appConfig);
  }
};
```

Let's break down what we've got:

- `resolveConfig`: Figure out which configuration to apply, and validate we didn't accidentally pass in something that can't exist.
- `config`: Resolve and return the correct configuration for the current environment.
- `setupSupergraph`: Helper for conditionally setting up a Supergraph stack, passing in the specifically narrowed down type for the Supergraph.
- `setupApp`: Helper for conditionally setting up an App stack, passing in the specifically narrowed down type for the App.


Now all that's left is to create our actual config, which is the one we'll be adjusting to our own needs. Create a new file `deployment/config.ts`:

```typescript
import type { ConfigMap, Config } from './lib/types';

const base: Config = {
  apps: [{ service: 'internal', subdomain: 'internal' }, { service: 'app' }],

  supergraph: {
    service: 'gateway',
    runtime: 'lambda',
    path: '/graphql',
    pinToVersionedApi: true,
  },

  subgraphs: [
    { name: 'users', project: 'ms-gql-users' },
    { name: 'products', project: 'ms-gql-products' },
    { name: 'reviews', project: 'ms-gql-reviews' },
  ],
};

const production: Config = {
  ...base,
  supergraph: {
    service: 'router',
    runtime: 'app-runner',
    path: '/graphql',
  },
};

export const config: ConfigMap = {
  Base: base,
  'Production Single-tenant': production,
  'Production Multi-tenant': production,
};
```

We configure our `Base` environment to use a Lambda Gateway which is cheaper, but we then opt for performance in our Production environments with the App Runner-based Apollo Router.

### Building artifacts in CI

### Deploying to App Runner

### Deploying to AWS Lambda



## Next Steps

Next up is to use our new API from our existing Frontend Apps! Follow along in Part 5 of the series (will be posted soon).
