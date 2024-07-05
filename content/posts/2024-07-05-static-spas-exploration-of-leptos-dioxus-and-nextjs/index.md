+++
title = "Static SPAs: Exploration of Leptos, Dioxus, and Next.js"
date = 2024-07-05
[taxonomies]
tags = ["rust", "wasm", "leptos", "dioxus", "nextjs", "spa"]
+++

One of my absolute favorite methods of deploying Frontends is to pre-generate all routes statically, and then let each route load the dependencies they need for interactivity.

- Load times are fast
- The user only downloads what they need (great for slower or mobile networks)
- SEO is great in many cases, and okay in others[^1]
- It can all still be deployed in the most optimal way which is CDN + Static Assets[^2]

I particularly care a lot about infrastructure scalability, maintainability, robustness, and cost. SSR typically makes each of these significantly worse compared to simply serving static files. I also don’t believe mixing your API into your Frontend code (i.e. [Server Actions](https://nextjs.org/docs/app/building-your-application/data-fetching/server-actions-and-mutations)) will do you any favors when you start scaling up your teams, but that’s a topic for another day.

The overall results:

| Framework | SSG Support | Hydration Support | Assets Chunking |
| --- | :---: | :---: | :---: |
| Next.js | ✅ | ✅ | ✅ Chunk per page |
| Leptos | ✅ | ✅ | ❌ One single WASM bundle* |
| Dioxus | ✅** | ✅ | ❌ One single WASM bundle* |

<div></div><!-- more -->

*: Support for chunking in WASM is generally poor, although recent strides have been made towards it (see [wasm-bindgen#3939](https://github.com/rustwasm/wasm-bindgen/issues/3939))

**: Dioxus support is in a state of flux, changing and improving how Static Generation is supported, leading to the docs and examples currently being broken (tracked in [dioxus#2587](https://github.com/DioxusLabs/dioxus/issues/2587))

{% aside() %}
You can see all the examples in the [https://github.com/Tehnix/comparison-ssg-rust-js](https://github.com/Tehnix/comparison-ssg-rust-js) repository.
{% end %}

In case it’s still not clear exactly what I mean with a “Static SPA”, let’s set up an example with Next.js, which supports this out of the box. The term for this is a bit messy, and you’ll find that React believes that this can still be called SSR where your rendering is just happening in compile time. Next.js will be calling this [SSG](https://nextjs.org/docs/pages/building-your-application/rendering/static-site-generation) in their Pages Router, and has confusingly changed the name of this in their newer App Router to be called [Static Exports](https://nextjs.org/docs/app/building-your-application/deploying/static-exports).

Let’s dive into the comparisons of how to achieve this across frameworks:

{{ toc() }}

[^1]: At least if that’s a concern and your App is not gated behind auth anyways, in which case the SEO aspect doesn’t matter at all

[^2]: As opposed to SSR (Server-side Rendering) which requires a server to be running that can generate and serve your assets

# Next.js SSG

Let’s set up [a new Next.js](https://nextjs.org/docs/getting-started/installation) project using the [Next.js 15 release candidate](https://nextjs.org/blog/next-15-rc#create-next-app-updates) so we can test all the latest advances for both React and Next.js.

We’re gonna accept most of the default recommendations:

```bash
$ bunx create-next-app@rc --turbo
✔ What is your project named? … next-example
✔ Would you like to use TypeScript? … Yes
✔ Would you like to use ESLint? … Yes
✔ Would you like to use Tailwind CSS? … Yes
✔ Would you like to use `src/` directory? … No
✔ Would you like to use App Router? (recommended) … Yes
✔ Would you like to use Turbopack for next dev? … Yes
✔ Would you like to customize the default import alias (@/*)? … No
Creating a new Next.js app in ./next-example.
```

If we `cd next-example`  and run `bun run dev` we’ll get a nice little start page:

{{ image(path="Screenshot_2024-07-02_at_12.06.50.png", caption="The default Next.js start page", width=600) }}

## Extending the default Next.js project

Now, for us to actually see anything interesting being generated later on, we’ll need to have more pages than just a single start page. Let’s add a few new pages with some test content.

We’ll set up two pages, reusing the styling of the main page, the first one in `next-example/app/sub-page/page.tsx` (create the missing `sub-page` directory):

```tsx ,linenos
"use client";

import { useState } from "react";

export default function SubPage() {
  const [counter, setCounter] = useState(0);
  return (
    // Reused styling.
    <div className="font-sans grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {/* Our content */}
        <h1>SubPage</h1>
        <p>This is a subpage with interactivity: {counter}</p>
        <button onClick={() => setCounter((prev) => prev + 1)}>
          Increment
        </button>
      </main>
    </div>
  );
}
```

And another similar one in `next-example/app/another-page/page.tsx` (create the missing `another-page` directory):

```tsx ,linenos
"use client";

import { useState, useEffect } from "react";

export default function AnotherPage() {
  const [windowHeight, setWindowHeight] = useState<number | undefined>(
    undefined
  );
  // Ensure that the window object is available before calling browser APIs.
  useEffect(() => {
    setWindowHeight(window.innerHeight);
  }, []);

  return (
    // Reused styling.
    <div className="font-sans grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {/* Our content */}
        <h1>Another Page</h1>
        <p>
          This is a another page calling browser APIs
          {windowHeight ? `: ${windowHeight}` : ""}
        </p>
      </main>
    </div>
  );
}
```

Note that in both of these examples we’re doing things that happen on the client-side (via `useState` and `useEffect`), so we need the `"use client";` pragma at the top.

And finally, we’ll link to these pages from our App entry point in `next-example/app/page.tsx`:

```tsx ,linenos,hl_lines=2 16 17
import Image from "next/image";
import Link from "next/link"; // This line here

export default function Home() {
  return (
    <div className="font-sans grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        <Image
          className="dark:invert"
          src="/next.svg"
          alt="Next.js logo"
          width={180}
          height={38}
          priority
        />
        <Link href="/sub-page">Sub page</Link> {/* <-- This line here */}
        <Link href="/another-page">Another page</Link> {/* <-- This line here */}
        {/* ...keep the rest of the content */}
```

We can run the development server to check that everything works via `bun run dev` in the `next-example` directory:


{{ images(paths=["Screenshot_2024-07-02_at_12.41.14.png", "Screenshot_2024-07-02_at_12.41.26.png", "Screenshot_2024-07-02_at_12.41.21.png"], captions=["Our updated start page", "The sub-page with an increment button", "The another-page showing the browser height"], defaultWidth=400) }}


Now that we have a few pages to render, using some interactivity and browser APIs, let’s take a look at how we’ll do that.

## Configuring SSG

The first change we’ll make will be to change our build output to be an [`export`](https://nextjs.org/docs/app/building-your-application/deploying/static-exports) which is what Next.js calls SSG in their App Router.

Open your `next-example/next.config.mjs` file and add the following lines:

```tsx ,linenos,hl_lines=3 5
/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export", // <-- This line here
  // Generate index.html files for each route in the output.
  trailingSlash: true, // <-- This line here
};

export default nextConfig;
```

The `trailingSlash` configuration will depend on your deployment method, but it essentially makes sure our simple static file server later on can serve the files without needing any logic to rewrite paths.

We can now inspect the generated build assets by running `bun run build` in the `next-example` directory

```bash
$ bun run build
...
Route (app)                              Size     First Load JS
┌ ○ /                                    13.3 kB         102 kB
├ ○ /_not-found                          900 B          89.8 kB
├ ○ /another-page                        882 B          89.8 kB
└ ○ /sub-page                            872 B          89.8 kB
+ First Load JS shared by all            88.9 kB
  ├ chunks/180-42348583fa4569ac.js       35.6 kB
  ├ chunks/4bd1b696-a08a63850fcad1d6.js  51.4 kB
  └ other shared chunks (total)          1.86 kB
```

Let’s check out what Next.js generated for us in the `out/` folder:

```bash
out
├── 404
│   └── index.html
├── 404.html
├── _next
│   ├── K46Mb3V6rvnSZkb3S0DFD
│   └── static
│       ├── K46Mb3V6rvnSZkb3S0DFD
│       │   ├── _buildManifest.js
│       │   └── _ssgManifest.js
│       ├── chunks
│       │   ├── 182-5aa8ba6aa9ca46c7.js
│       │   ├── 4bd1b696-3f85179bbee9de79.js
│       │   ├── 932-e409b1b1d42740a9.js
│       │   ├── app
│       │   │   ├── _not-found
│       │   │   │   └── page-1477ca64449fa6ca.js
│       │   │   ├── another-page
│       │   │   │   └── page-aa4b7b15eb983969.js
│       │   │   ├── layout-94b4bb4f7b4ed0fc.js
│       │   │   ├── page-18c8594afae77168.js
│       │   │   └── sub-page
│       │   │       └── page-b5ac62c0a67a677a.js
│       │   ├── framework-d2f4bc65ced8d4a1.js
│       │   ├── main-286537da132b2fda.js
│       │   ├── main-app-5fa6097c9a9b5d34.js
│       │   ├── pages
│       │   │   ├── _app-daa5cb8560567b4d.js
│       │   │   └── _error-4cacf33de97c3163.js
│       │   ├── polyfills-78c92fac7aa8fdd8.js
│       │   └── webpack-d22356f1f4db00c4.js
│       ├── css
│       │   └── ba86f67683758cf0.css
│       └── media
│           ├── 4473ecc91f70f139-s.p.woff
│           └── 463dafcda517f24f-s.p.woff
├── another-page
│   ├── index.html
│   └── index.txt
├── favicon.ico
├── file-text.svg
├── globe.svg
├── index.html
├── index.txt
├── next.svg
├── sub-page
│   ├── index.html
│   └── index.txt
├── vercel.svg
└── window.svg
```

The interesting files here are:

- `out/index.html`: The generated HTML for our starting page
- `out/sub-page/index.html`: The generated HTML for our sub-page
- `out/_next/static/chunks/app/sub-page/page-b5ac62c0a67a677a.js`: The JavaScript that is specific to our sub-page, and will only be loaded on that page
- `out/another-page/index.html`: The generated HTML for our another-page
- `out/_next/static/chunks/app/another-page/page-aa4b7b15eb983969.js`: The JavaScript that is specific to our another-page

The chunking of the JavaScript into smaller files, some shared and some page specific, is great for our users and will mean they’ll only load the minimal JavaScript that they need to interact with the page they are on.

{% aside() %}
The chunked files are hashed, so if your file hasn’t changed it will get the same hash which helps cache hits in the user’s browser.
{% end %}

## Testing with JavaScript disabled

We can test our site in Chrome [without JavaScript](https://developer.chrome.com/docs/devtools/javascript/disable) by opening the Chrome DevTools, hitting CMD + Shift + P, and typing in “Disable JavaScript”

{{ image(path="Screenshot_2024-07-02_at_12.51.40.png", caption="Disable JavaScript via the Chrome DevTools", width=600) }}

We can then serve our static files by via `cd next-example/out && bunx simplehttpserver`. Let’s make some comparisons, running with JavaScript and with JavaScript disabled.

First, our start page. Looking exactly the same, with all links working and no other changes in functionality since we didn’t have anything interactive here:


{{ images(paths=["Screenshot_2024-07-02_at_12.58.54.png", "Screenshot_2024-07-02_at_13.02.27.png"], captions=["Start page with JavaScript enabled", "Start page with JavaScript disabled"], defaultWidth=400) }}

Our sub-page will look exactly the same, but once you try to interact with the `Increment` button when JavaScript is disabled it obviously will not work:

{{ images(paths=["Screenshot_2024-07-02_at_12.59.00.png", "Screenshot_2024-07-02_at_13.06.28.png"], captions=["sub-page with JavaScript enabled", "sub-page with JavaScript disabled"], defaultWidth=400) }}

We do notice a tiny difference in our another, specifically it will never call the `useEffect` to get the browser height when JavaScript is disabled. Nothing surprising here:

{{ images(paths=["Screenshot_2024-07-02_at_12.59.03.png", "Screenshot_2024-07-02_at_13.06.33.png"], captions=["another-page with JavaScript enabled", "another-page with JavaScript disabled"], defaultWidth=400) }}

The above should demonstrate exactly the benefit of this approach: We get most of the benefits of SSR without paying the costs. SEO crawlers will be able to read most of our content, except for the dynamic parts.

There’s a lot more you can do in the static exports, e.g. generate dynamic routes such as pre-generating all your i18n routes for each language.

I have an example of that in the Next.js setup I use in this post: [https://codethoughts.io/posts/2023-10-16-the-stack-part-3/#next-js](https://codethoughts.io/posts/2023-10-16-the-stack-part-3/#next-js).

## Optional configuration

If you’re looking for some additional recommendations to make your Next.js site better for your users, I’d recommend the following.

Enable the experimental support for the React Compiler, by first installing it:

```tsx
$ bun install --dev babel-plugin-react-compiler
```

And updating the `next-example/next.config.mjs`:

```tsx ,linenos,hl_lines=8
/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  // Generate index.html files for each route in the output.
  trailingSlash: true,
  // Optional: Enable experimental React Compiler support.
  experimental: {
    reactCompiler: true, // <-- This line here
  },
};

export default nextConfig;

```

## Further Reading

It’s worth reading the notes on [supported features](https://nextjs.org/docs/app/building-your-application/deploying/static-exports#supported-features) as well as [unsupported features](https://nextjs.org/docs/app/building-your-application/deploying/static-exports#unsupported-features). Here are a few snippets:

> **Server Components:** When you run `next build` to generate a static export, Server Components consumed inside the `app` directory will run during the build, similar to traditional static-site generation.

and:

> **Client Components:** If you want to perform data fetching on the client, you can use a Client Component with [SWR](https://github.com/vercel/swr) to memoize requests.

as well as:

> **Browser APIs:** Client Components are pre-rendered to HTML during `next build`. Because [Web APIs](https://developer.mozilla.org/docs/Web/API) like `window`, `localStorage`, and `navigator` are not available on the server, you need to safely access these APIs only when running in the browser.

# Leptos SSG

Now that we have a clear concrete idea of what our goal is and why we are trying to achieve it, let’s see if we can make a similar setup using Leptos and WASM. We unfortunately can’t test the new 0.7 alpha since static routes are yet to be supported.

We do however need to be at least on version TBD, which includes my fix for generating sub-routes with trailing slashes correctly ([leptos#2667](https://github.com/leptos-rs/leptos/pull/2667)).

If you don’t already have [cargo-leptos](https://github.com/leptos-rs/cargo-leptos) and [cargo-generate](https://github.com/cargo-generate/cargo-generate), we’ll set that up first so that we can use the [leptos-rs/start-axum](https://github.com/leptos-rs/start-axum) starter template:

```bash
$ cargo binstall cargo-generate --yes # Or: cargo install cargo-generate
$ cargo binstall cargo-leptos # Or: cargo install cargo-leptos --locked
```

We can now get started with our Leptos project, calling it `leptos-example`:

```bash
$ cargo leptos new --git leptos-rs/start-axum
🤷   Project Name: leptos-example
🤷   Use nightly features? · Yes
```

Let’s `cd leptos-example` and finish setting up the project by adding our compiler targets using rustup:

```bash
$ rustup toolchain install nightly --allow-downgrade
$ rustup target add wasm32-unknown-unknown
```

And finally, we’ll want the generated files to be hashed. Edit your `leptos-example/Cargo.toml`:

```toml ,linenos,hl_lines=6
# ...add to your existing [package.metadata.leptos]
[package.metadata.leptos]
# Enables additional file hashes on outputted css, js, and wasm files
#
# Optional: Defaults to false. Can also be set with the LEPTOS_HASH_FILES=false env var (must be set at runtime too)
hash-files = true
```

If we run `cd leptos-example` and run `cargo leptos watch` we get a nice little start page:

![The default Leptos start page using the start-axum template.](Static%20SPAs%20Exploration%20of%20Leptos,%20Dioxus,%20and%20Nex%20d982c0fe0f0e4170b2f2a7c81040d755/Screenshot_2024-07-02_at_14.14.10.png)

The default Leptos start page using the start-axum template.

To avoid your eyes bleeding from the default white background, you can update the default `leptos-example/style/main.scss` to:

```scss ,linenos
body {
  font-family: sans-serif;
  text-align: center;
  background: #0a0a0a;
  color: #ededed;
}
```

## Extending the default Leptos project

Just like with Next.js, for us to actually see anything interesting being generated later on, we’ll need to have more pages than just a single start page. Let’s add a few new pages with some test content.

We’ll set up two pages, the first one in `leptos-example/src/subpage.rs`, mimicking the equivalent Next.js page:

```rust ,linenos
use leptos::*;

#[component]
pub fn SubPage() -> impl IntoView {
    // Creates a reactive value to update the button
    let (count, set_count) = create_signal(0);
    let on_click = move |_| set_count.update(|count| *count += 1);

    view! {
        <h1>"SubPage"</h1>
        <p>"This is a subpage with interactivity: " {count}</p>
        <button on:click=on_click>"Increment"</button>
    }
}
```

Similarly, we’ll set up in `leptos-example/src/anotherpage.rs`, mimicking the equivalent Next.js page:

```rust ,linenos
use leptos::*;
use leptos_dom::window;

#[component]
pub fn AnotherPage() -> impl IntoView {
    let (window_height, set_window_height) = create_signal::<Option<f64>>(None);
    let window_text = move || {
        window_height()
            .map(|w| format!(": {}", w))
            .unwrap_or("".to_owned())
    };

    // Ensure that the window object is available before calling browser APIs.
    create_effect(move |_| {
        set_window_height(window().inner_height().ok().map(|w| w.as_f64().unwrap()));
    });

    view! {
        <h1>"Another Page"</h1>
        <p>"This is a another page calling browser APIs" {window_text}</p>
    }
}
```

We’ll also need to update our `leptos-example/src/lib.rs` to make these pages accessible in our project:

```rust ,linenos,hl_lines=5 6
pub mod app;
pub mod error_template;
#[cfg(feature = "ssr")]
pub mod fileserv;
pub mod subpage; // This line here
pub mod anotherpage; // This line here

// ...the rest of the file remains the same
```

And finally, add these pages to our Routes with a link by editing `leptos-example/src/app.rs` by first importing them:

```rust ,linenos,hl_lines=5 6
use crate::error_template::{AppError, ErrorTemplate};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use crate::subpage::SubPage; // This line here
use crate::anotherpage::AnotherPage; // This line here
// ...
```

And then extending our `Routes` in the `App` component:

```rust ,linenos,hl_lines=4 5
                // ...add the routes in our App component.
                <Routes>
                    <Route path="" view=HomePage/>
                    <Route path="/sub-page" view=SubPage/> // This line here
                    <Route path="/another-page" view=AnotherPage/> // This line here
                </Routes>
                // ...
```

And finally, extend our `HomePage` component, adding the links:

```rust ,linenos,hl_lines=10 11
#[component]
fn HomePage() -> impl IntoView {
    // Creates a reactive value to update the button
    let (count, set_count) = create_signal(0);
    let on_click = move |_| set_count.update(|count| *count += 1);

    view! {
        <h1>"Welcome to Leptos!"</h1>
        <button on:click=on_click>"Click Me: " {count}</button>
        <A href="/sub-page">"Sub page"</A> // This line here
        <A href="/another-page">"Another page"</A> // This line here
    }
}
```

We can run the development server to check that everything works via `cargo leptos watch` in the `leptos-example` directory:


{{ images(paths=["Screenshot_2024-07-02_at_15.03.48.png", "Screenshot_2024-07-02_at_15.03.50.png", "Screenshot_2024-07-02_at_15.18.09.png"], captions=["Our updated start page", "The sub-page with an increment button", "The another-page showing the browser height"], defaultWidth=400) }}

Now that we have a few pages to render, using some interactivity and browser APIs, let’s take a look at how we’ll do that.

## Configuring SSG

While static generation is a bit underdocumented in Leptos, we can get still manage by following along with [Greg’s comment here](https://github.com/leptos-rs/leptos/issues/1594#issuecomment-1845939151) as well as the [0.5 release notes](https://github.com/leptos-rs/leptos/releases/tag/v0.5.0) on where Static Site Generation was announced.

The first thing we’ll do is to change our `Routes` to be `StaticRoute`s instead. Edit the `leptos-example/src/app.rs` file, changing our `Routes` to look like this instead:

```rust ,linenos,hl_lines=3-22
				        // ...change the routes in our App component.
                <Routes>
                    <StaticRoute
                        mode=StaticMode::Upfront
                        path=""
                        view=HomePage
                        static_params=move || Box::pin(async move { StaticParamsMap::default() })
                    /> // This line here
                    <StaticRoute
                        mode=StaticMode::Upfront
                        path="/sub-page/"
                        trailing_slash=TrailingSlash::Exact
                        view=SubPage
                        static_params=move || Box::pin(async move { StaticParamsMap::default() })
                    /> // This line here
                    <StaticRoute
                        mode=StaticMode::Upfront
                        path="/another-page/"
                        trailing_slash=TrailingSlash::Exact
                        view=AnotherPage
                        static_params=move || Box::pin(async move { StaticParamsMap::default() })
                    /> // This line here
                </Routes>
                // ...
```

Both the `trailing_slash=TrailingSlash::Exact` and the `/` on the routes are important for Leptos to build the files in the correct location and way we expect.

We also need to tell Leptos to generate the output for these files, which we’ll do by updating our `leptos-example/src/main.rs` with some additional imports so that we can call `build_static_routes`:

```rust ,linenos,hl_lines=5 7 8
#[cfg(feature = "ssr")]
#[tokio::main]
async fn main() {
    // ...update our imports with generate_route_list_with_ssg and build_static_routes
    use leptos_axum::{generate_route_list_with_ssg, build_static_routes, LeptosRoutes};
    // ...replace the existing let routes ... with this
    let (routes, static_data_map) = generate_route_list_with_ssg(App); // This line here
    build_static_routes(&leptos_options, App, &routes, static_data_map).await; // This line here
    // ...the rest of the file remains the same
}
```

This will generate the static files each time our entry function runs.

We can now inspect the generated build assets by running `LEPTOS_HASH_FILES=true cargo leptos build --release` following by running the binary via `LEPTOS_HASH_FILES=true ./target/release/leptos-example` in the `leptos-example` directory (you might have to exit it manually with Ctrl-C):

```bash
$ LEPTOS_HASH_FILES=true ./target/release/leptos-example
building static route: /sub-page
listening on http://127.0.0.1:3000
building static route: /another-page
building static route:
# ..exit the process after you've seen it build the routes
```

Let’s check out what Next.js generated for us in the `target/site/` folder:

```bash
$ tree target/site
target/site
├── another-page
│   └── index.html
├── favicon.ico
├── index.html
├── pkg
│   ├── leptos-example.8RfUbHBiJMoruMHObx2F7Q.css
│   ├── leptos-example.XcFzle8Cx3F6iOeNDhvIAw.js
│   └── leptos-example.p9VEDheNIWNS98tonwfhvw.wasm
└── sub-page
    └── index.html
```

## Testing with JavaScript disabled

Same approach as last, we’ll test our site in Chrome [without JavaScript](https://developer.chrome.com/docs/devtools/javascript/disable) by opening the Chrome DevTools, hitting CMD + Shift + P, and typing in “Disable JavaScript”

{{ image(path="Screenshot_2024-07-02_at_12.51.40.png", caption="Disable JavaScript via the Chrome DevTools", width=600) }}

We can then serve our static files by via `cd leptos-example/target/site && bunx simplehttpserver`. Let’s make some comparisons, running with JavaScript and with JavaScript disabled.

First, our start page. Looking exactly the same, with all links working and no other changes in functionality since we didn’t have anything interactive here:

{{ images(paths=["Screenshot_2024-07-02_at_15.46.31.png", "Screenshot_2024-07-02_at_15.47.52.png"], captions=["Start page with JavaScript enabled", "Start page with JavaScript disabled"], defaultWidth=400) }}

Our sub-page will look exactly the same, but once you try to interact with the `Increment` button when JavaScript is disabled it obviously will not work:

{{ images(paths=["Screenshot_2024-07-02_at_15.47.18.png", "Screenshot_2024-07-02_at_16.06.34.png"], captions=["sub-page with JavaScript enabled", "sub-page with JavaScript disabled"], defaultWidth=400) }}

We do notice a tiny difference in our another, specifically it will never call the `create_effect` to get the browser height when JavaScript is disabled. Nothing surprising here:

{{ images(paths=["Screenshot_2024-07-02_at_15.47.32.png", "Screenshot_2024-07-02_at_16.06.31.png"], captions=["another-page with JavaScript enabled", "another-page with JavaScript disabled"], defaultWidth=400) }}

Very similar results to Next.js!

# Dioxus SSG

Let’s explore another Rust-based framework, Dioxus, to see how SSG is supported and how well it works.

We’ll start by setting up the Dioxus CLI:

```bash
# $ cargo binstall dioxus-cli # Or: cargo install dioxus-cli
$ cargo install --git https://github.com/DioxusLabs/dioxus dioxus-cli
```

We can then use the `dx` CLI to create our new project, which we’ll set up as a fullstack project which can “renders to HTML text on the server and hydrates it on the client”[^dioxus-getting-started]:

```bash
$ dx new
✔ 🤷   Which sub-template should be expanded? · Fullstack
  🤷   Project Name: dioxus-example
✔ 🤷   Should the application use the Dioxus router? · true
✔ 🤷   How do you want to create CSS? · Tailwind
```

If you chose TailwindCSS, you won’t have any default styling. Let’s make a small improvement to `dioxus-example/input.css`, which is where the Tailwind styles will be picked up from. That’ll look like this:

```scss ,linenos
@tailwind base;
@tailwind components;
@tailwind utilities;

body {
  @apply bg-black text-white text-center justify-center mt-16;
  a {
    @apply underline mx-4;
  }
  button {
    @apply bg-neutral-700 p-2 m-4;
  }
}
```

And then run `bunx tailwindcss -i ./input.css -o ./assets/tailwind.css` in the `dioxus-example` directory to get the styles compiled into `dioxus-example/assets/tailwind.css`.

You can also safely delete the alternative `dioxus-example/assets/main.css` which would have been used if we hadn’t chosen TailwindCSS.

Finally, we’ll add the `web-sys` package to our dependencies in `dioxus-example/Cargo.toml`, which we’ll need later:

```toml ,linenos,hl_lines=6
[package]
# ...

[dependencies]
# Add web-sys crate to interact with Browser APIs.
web-sys = { version = "0.3", features = ["Window"] }
# ...the rest of the file remains the same
```

If we now `cd dioxus-example` and run `dx serve --platform fullstack` we get a nice little start page:

![The default Dioxus start page using the fullstack template.](Static%20SPAs%20Exploration%20of%20Leptos,%20Dioxus,%20and%20Nex%20d982c0fe0f0e4170b2f2a7c81040d755/Screenshot_2024-07-03_at_10.56.20.png)

The default Dioxus start page using the fullstack template.

[^dioxus-getting-started]: [https://dioxuslabs.com/learn/0.5/getting_started#create-a-new-project](https://dioxuslabs.com/learn/0.5/getting_started#create-a-new-project)

## Extending the default Dioxus project

Once again, for us to actually see anything interesting being generated later on, we’ll need to have more pages than just a single start page[^dioxus-default-startpage]. Let’s add a few new pages with some test content.

We’ll set up two pages, the first one in `dioxus-example/src/subpage.rs`, mimicking the equivalent Next.js page:

```rust ,linenos
use dioxus::prelude::*;

#[component]
pub fn SubPage() -> Element {
    // Creates a reactive value to update the button
    let mut count = use_signal(|| 0);

    rsx! {
        h1 { "SubPage" }
        p { "This is a subpage with interactivity: {count}" }
        button { onclick: move |_| count += 1, "Increment" }
    }
}
```

Similarly, we’ll set up in `dioxus-example/src/anotherpage.rs`, mimicking the equivalent Next.js page:

```rust ,linenos
use dioxus::prelude::*;
use web_sys::window;

#[component]
pub fn AnotherPage() -> Element {
    let mut window_height = use_signal::<Option<f64>>(|| None);
    let window_text = use_memo(move || {
        window_height()
            .map(|w| format!(": {}", w))
            .unwrap_or("".to_owned())
    });

    // Ensure that the window object is available before calling browser APIs.
    use_effect(move || {
        let window = window();
        if let Some(w) = window {
            window_height.set(w.inner_height().ok().map(|w| w.as_f64().unwrap()));
        }
    });
    rsx! {
        h1 { "Another Page" }
        p { "This is a another page calling browser APIs{window_text}" }
    }
}
```

We can now integrate these pages into our App in `dioxus-example/src/main.rs`. First, we’ll change our `Route` enum to have our new pages, and remove the default blog page:

```rust ,linenos,hl_lines=6-9
// ...
#[derive(Clone, Routable, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
enum Route {
    #[route("/")]
    Home {},
    #[route("/sub-page")] // This line here
    SubPage {}, // This line here
    #[route("/another-page")] // This line here
    AnotherPage {}, // This line here
}
// ...
```

Then we’ll update our `Home` component to link to the pages, and remove the link to the blog page as well as the `Blog` component:

```rust ,linenos,hl_lines=7 8
#[component]
fn Home() -> Element {
    let mut count = use_signal(|| 0);
    let mut text = use_signal(|| String::from("..."));

    rsx! {
        Link { to: Route::SubPage {}, "Sub page" } // This line here
        Link { to: Route::AnotherPage {}, "Another page" } // This line here
        div {
            h1 { "High-Five counter: {count}" }
            button { onclick: move |_| count += 1, "Up high!" }
            button { onclick: move |_| count -= 1, "Down low!" }
            button {
                onclick: move |_| async move {
                    if let Ok(data) = get_server_data().await {
                        tracing::info!("Client received: {}", data);
                        text.set(data.clone());
                        post_server_data(data).await.unwrap();
                    }
                },
                "Get Server Data"
            }
            p { "Server data: {text}"}
        }
    }
}
```

We can run the development server to check that everything works via `dx serve --platform fullstack` in the `dioxus-example` directory:

{{ images(paths=["Screenshot_2024-07-03_at_15.14.12.png", "Screenshot_2024-07-03_at_11.21.27.png", "Screenshot_2024-07-03_at_15.10.15.png"], captions=["Our updated start page", "The sub-page with an increment button", "The another-page showing the browser height"], defaultWidth=400) }}

Now that we have a few pages to render, using some interactivity and browser APIs, let’s take a look at how we’ll do that.

[^dioxus-default-startpage]: Dioxus does add a sub-route by default, but we’ll ignore that to make the comparisons equal

## Configuring SSG

{% aside(icon="⚠️") %}
We’ll have to use the bleeding edge, since we want to utilize the recently merged [dioxus-static-site-generation](https://github.com/DioxusLabs/dioxus/tree/main/packages/static-generation) crate, introduced in [dioxus#2226](https://github.com/DioxusLabs/dioxus/pull/2226), which has not yet been included in any releases (latest being 0.5.1 as of writing).
{% end %}

Until there’s a new release of Dioxus, we’ll have to install the CLI from git, to get the latest features:

```rust
cargo install --git https://github.com/DioxusLabs/dioxus dioxus-cli
```

We’ll also update our dependencies to point `dioxus` to a working commit where static site generation is included in [245003a5d430ab8e368094cd32208178183fc24e](https://github.com/DioxusLabs/dioxus/commit/245003a5d430ab8e368094cd32208178183fc24e) in `dioxus-example/Cargo.toml`, as well as replace the `fullstack` feature with the `static-generation` feature:

```toml ,linenos,hl_lines=6-9
[package]
# ...

[dependencies]
# Update dixous to use this specific commit 245003a5d430ab8e368094cd32208178183fc24e
dioxus = { git = "https://github.com/DioxusLabs/dioxus", rev = "245003a5d430ab8e368094cd32208178183fc24e", features = [
  "static-generation",
  "router",
] }
# ...the rest of the file remains the same
```

We can now replace both the default `main` functions with a very simple one, as well as delete the default `post_server_data` and `get_server_data` functions. Your `dioxus-example/src/main.rs` should now look like this:

```rust ,linenos
#![allow(non_snake_case)]

use dioxus::prelude::*;

mod subpage;
use subpage::SubPage;
mod anotherpage;
use anotherpage::AnotherPage;

#[derive(Clone, Routable, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
enum Route {
    #[route("/")]
    Home {},
    #[route("/sub-page")]
    SubPage {},
    #[route("/another-page")]
    AnotherPage {},
}

// Generate all routes and output them to the static path
fn main() {
    launch(App);
}

fn App() -> Element {
    rsx! {
        Router::<Route> {}
    }
}

#[component]
fn Home() -> Element {
    let mut count = use_signal(|| 0);

    rsx! {
        Link { to: Route::SubPage {}, "Sub page" }
        Link { to: Route::AnotherPage {}, "Another page" }
        div {
            h1 { "High-Five counter: {count}" }
            button { onclick: move |_| count += 1, "Up high!" }
            button { onclick: move |_| count -= 1, "Down low!" }
        }
    }
}
```

Dioxus knows which `launch` function to call, based on the feature flags enabled, which in our case is `static-generation`.

Finally, we’ll need to adjust the default path to our generated `tailwind.css` file to make sure that sub-routes load it correctly from the root and not relative to themselves. Adjust the `style` setting in `dioxus-example/Dioxus.toml`:

```toml ,linenos,hl_lines=3
# ...
[web.resource]
style = ["/tailwind.css"]
# ...the rest of the file remains the same
```

We can now inspect the generated build assets by running `dx build --platform fullstack --release` following by running the binary via `./dist/dioxus-example` in the `dioxus-example` directory:

```bash ,linenos
$ dx build --platform fullstack --release
...
build desktop done
$ ./dist/dioxus-example
```

Let’s check out what Next.js generated for us in the `static/` folder (not the `dist/` folder!):

```bash
$ tree static
static
├── __assets_head.html
├── another-page
│   └── index.html
├── assets
│   └── dioxus
│       ├── dioxus-example.js
│       ├── dioxus-example_bg.wasm
│       └── snippets
│           ├── dioxus-interpreter-js-7c1300c6684e1811
│           │   ├── inline0.js
│           │   └── src
│           │       └── js
│           │           └── common.js
│           ├── dioxus-interpreter-js-9ac3b5e174d5b843
│           │   ├── inline0.js
│           │   └── src
│           │       └── js
│           │           ├── common.js
│           │           └── eval.js
│           ├── dioxus-web-84af743b887ebc54
│           │   ├── inline0.js
│           │   ├── inline1.js
│           │   └── src
│           │       └── eval.js
│           └── dioxus-web-90b865b1369c74f4
│               ├── inline0.js
│               └── inline1.js
├── dioxus-example
├── favicon.ico
├── header.svg
├── index.html
├── sub-page
│   └── index.html
└── tailwind.css
```

## Testing with JavaScript disabled

Same approach as last, we’ll test our site in Chrome [without JavaScript](https://developer.chrome.com/docs/devtools/javascript/disable) by opening the Chrome DevTools, hitting CMD + Shift + P, and typing in “Disable JavaScript”

{{ image(path="Screenshot_2024-07-02_at_12.51.40.png", caption="Disable JavaScript via the Chrome DevTools", width=600) }}

We can then serve our static files by via `cd dioxus-example/static && bunx simplehttpserver`. Let’s make some comparisons, running with JavaScript and with JavaScript disabled.

First, our start page. Looking exactly the same, with all links working and no other changes in functionality since we didn’t have anything interactive here:

{{ images(paths=["Screenshot_2024-07-05_at_20.52.40.png", "Screenshot_2024-07-05_at_20.54.23.png"], captions=["Start page with JavaScript enabled", "Start page with JavaScript disabled"], defaultWidth=400) }}

Our sub-page will look exactly the same, but once you try to interact with the `Increment` button when JavaScript is disabled it obviously will not work:

{{ images(paths=["Screenshot_2024-07-05_at_20.52.47.png", "Screenshot_2024-07-05_at_21.02.08.png"], captions=["sub-page with JavaScript enabled", "sub-page with JavaScript disabled"], defaultWidth=400) }}

We do notice a tiny difference in our another, specifically it will never call the `use_effect` to get the browser height when JavaScript is disabled. Nothing surprising here:

{{ images(paths=["Screenshot_2024-07-05_at_20.52.51.png", "Screenshot_2024-07-05_at_21.02.11.png"], captions=["another-page with JavaScript enabled", "another-page with JavaScript disabled"], defaultWidth=400) }}

# Conclusion

While Next.js certainly had the smoothest path and the best support for SSG, especially with the benefit of asset chunking, the Rust-based frameworks show a viable path forward if you are committed to staying in the Rust ecosystem.

I personally found Leptos easier to set up and work with, but either framework is progressing very fast, so it’s a bit more up to personal preference.

As mentioned in the beginning of the post, our end results ended up being:

| Framework | SSG Support | Hydration Support | Assets Chunking |
| --- | :---: | :---: | :---: |
| Next.js | ✅ | ✅ | ✅ Chunk per page |
| Leptos | ✅ | ✅ | ❌ One single WASM bundle* |
| Dioxus | ✅** | ✅ | ❌ One single WASM bundle* |

*: Support for chunking in WASM is generally poor, although recent strides have been made towards it (see [wasm-bindgen#3939](https://github.com/rustwasm/wasm-bindgen/issues/3939))

**: Dioxus support is in a state of flux, changing and improving how Static Generation is supported, leading to the docs and examples currently being broken (tracked in [dioxus#2587](https://github.com/DioxusLabs/dioxus/issues/2587))
