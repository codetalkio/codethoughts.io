---
title: "The Stack Part 3: Building a Frontend"
tags: aws, cloud, infrastructure, cdk
---

In [the last post](/posts/2023-10-07-the-stack-part-2.html) we set up our deployment, fully automated on merge to our `main` branch. In this post we will be building our UI (Frontend) applications. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

At the end of this post we will have:

- A [Next.js](https://nextjs.org/) Frontend app with support for localization, using Tailwind CSS.
- A [Leptos](https://github.com/leptos-rs/leptos) Rust/WASM Frontend app with support for localization, using Tailwind CSS.
- Automatic deployment of our Apps AWS using CDK, statically hosted using S3 + CloudFront.

We are essentially hedging our bets by building both a JavaScript-based Frontend, which is the safe bet, and a Rust/WASM-based Frontend, which is the future bet. We will be using the same GraphQL API for both, so we can easily switch between them.

If you want to jump straight to the code, you can find it in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-3-frontend) which links directly to the Part 3 branch.

<div></div><!--more-->

## Prelude: Static Site Generation

While SSR (Server Side Rendering) is seeing a renaissance these days, we are intentionally avoiding this functionality. If you remember the architecture outlined in [our introduction to the series](/posts/2023-01-29-the-stack.html#what-will-we-be-covering), we want to be able to serve our Frontend entirely using static file hosting.

There are multiple reasons for this, going back to our core design goals:

- **Low cost**: Static file hosting is extremely cheap, and scales as well as S3/CloudFront does (i.e. *very well*).
- **Low operational overhead**: Static file hosting is extremely simple to operate—there are no servers for us to worry about and scale up/down as needed, no need for any orchestration yet.
- **Performant**: Static file hosting is extremely performant, as the files are served directly from the edge, and not from a server.

We do sacrifice the ability to do SSR and the various User Experiences that can potentially bring, but the benefits are far outweighed by the downsides in this tradeoff.

## Next.js

Next.js is one of the most popular React frameworks at the moment, and supports a lot of niceties as well as sets us on a path of a good structure from the get-go. It has a lot of momentum behind it, a strong focus on the Developer Experience, and uses React which makes it a very familiar option for a large majority of Frontend Engineers out there.

We'll once again use [Bun](https://bun.sh/), which you can install via:

```bash
$ curl -fsSL https://bun.sh/install | bash
```

While [bun unfortunately doesn't fully support the Next.js App Router yet](https://bun.sh/guides/ecosystem/nextjs) we will still rely on it for installing dependencies and being our general go-to tool for running anything JS related.

We'll end up with the following application at the end:

```bash
👉 INSERT SCREENSHOT HERE 👈
```

#### Setting up our Next.js App

Let's get our Next.js app set up, which we will call `ui-app`:

```bash
$ bun create next-app # Call it ui-app
✔ What is your project named? … ui-app
✔ Would you like to use TypeScript? … Yes
✔ Would you like to use ESLint? … Yes
✔ Would you like to use Tailwind CSS? … Yes
✔ Would you like to use `src/` directory? … Yes
✔ Would you like to use App Router? (recommended) … Yes
✔ Would you like to customize the default import alias (@/*)? … No
```

This gets us quite far, we now have an App we can run, Tailwind CSS is already set up, and we got a lot of the structure set up for us:

```bash
$ cd ui-app
$ bun run dev # or bun run build
next dev
  ▲ Next.js 13.5.4
  - Local:        http://localhost:3000

 ✓ Ready in 3s
```

Voila, we've got a little Hello World Next.js app!

#### Building Static Files

We need to do just one small change to our Next.js setup to make it output static files for us. We'll do this by adding `output: "export"` to our `next.config.js` file at the root of `ui-app/`:

```typescript
/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
};

module.exports = nextConfig;
```

This tells Next.js that we want to to [Statically Export](https://nextjs.org/docs/app/building-your-application/deploying/static-exports) our files, which will generate an HTML file per route, which allows each route to serve the minimal content it needs, enabling faster page loads, instead of the traditional SPA approach of serving one large file upfront.

We get the best of both worlds here, as we still get the reduced bundle sizes typical of SSR, but can retain the static file advantage of SPAs.

#### Setting up Localization



## Leptos (Rust/WASM)

Leptos is one of the newer entries on the Rust/WASM scene. It has a radical focus on performance and scalability of your codebase, so that as your codebase grows your App doesn't just start to become slower which is a typical issue in React and VDOM-based frameworks.

Leptos should feel somewhat familiar, although it is more closely related to something like [Solid.js](https://www.solidjs.com/) which is based on Signals and not using a VDOM. Leptos has a good quick overview of features [here](https://github.com/leptos-rs/leptos#what-does-that-mean) and a nice [FAQ here](https://github.com/leptos-rs/leptos#faqs).

We'll end up with the following application at the end:

```bash
👉 INSERT SCREENSHOT HERE 👈
```

#### Setting up our Leptos App

We will be using Trunk for developing and building our Leptos App. Trunk is a great tool for developing Rust/WASM Apps, and is very similar to Bun (in a sense) in that it is a wrapper around the underlying tools. Let's install it first:

```bash
# Install dependencies
$ cargo install trunk
```

We can then set up our project, which we'll call `ui-internal`:

```bash
$ cargo init ui-internal
$ cd ui-internal
# Add dependencies and set up our toolchain
$ cargo add leptos --features=csr,nightly
$ rustup toolchain install nightly
$ rustup default nightly
$ rustup target add wasm32-unknown-unknown
```

And finally, we'll use Rust Nightly to develop our App, which gives us a few better ergonomics:

```bash
$ rustup toolchain install nightly
$ rustup default nightly
$ rustup target add wasm32-unknown-unknown
```

Let's create a quick `index.html` file in the root of the `ui-internal/` folder, just to get started:

```html
<!DOCTYPE html>
<html>
  <head></head>
  <body></body>
</html>
```

And replace the contents of our `src/main.rs`:

```rust
use leptos::*;

fn main() {
    mount_to_body(|| view! { <p>"Hello, world!"</p> })
}
```

We can now run our App using Trunk:

```bash
$ trunk serve --open
```

Voila, we've got a little Hello World Leptos app!

#### Setting up Tailwind CSS

#### Setting up Localization

## Automating Deployments via CDK


## Next Steps

Next up is to set up our Federated GraphQL API! Follow along in Part 4 of the series (will be posted soon).
