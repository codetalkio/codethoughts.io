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

- [Prelude: Static Site Generation](#prelude-static-site-generation)
- [Next.js](#nextjs)
    - [Setting up our Next.js App](#setting-up-our-nextjs-app)
    - [Building Static Files](#building-static-files)
    - [Setting up Localization](#setting-up-localization)
- [Leptos (Rust/WASM)](#leptos-rustwasm)
    - [Setting up our Leptos App](#setting-up-our-leptos-app)
    - [Setting up Tailwind CSS](#setting-up-tailwind-css)
    - [Setting up Localization](#setting-up-localization-1)
- [Automating Deployments via CDK](#automating-deployments-via-cdk)
- [Bonus: End-to-End Tests](#bonus-end-to-end-tests)
- [Bonus: DevEx Improvements](#bonus-devex-improvements)
- [Next Steps](#next-steps)


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
// @ts-check

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  experimental: {
    // Statically type links to prevent typos and other errors when using next/link, improving type safety when navigating between pages.
    typedRoutes: true,
  },
};

module.exports = nextConfig;
```

This tells Next.js that we want to to [Statically Export](https://nextjs.org/docs/app/building-your-application/deploying/static-exports) our files, which will generate an HTML file per route, which allows each route to serve the minimal content it needs, enabling faster page loads, instead of the traditional SPA approach of serving one large file upfront.

We get the best of both worlds here, as we still get the reduced bundle sizes typical of SSR, but can retain the static file advantage of SPAs.

#### Setting up Localization

As our customer-base grows, we will inevitably run into the need to localization. To do this, we will restructure our App with basic support for this, as well as bring in a dependency to help us with this, namely [next-intl](https://www.npmjs.com/package/next-intl). Vercel also has some good documentation on how to [Get Started](https://next-intl-docs.vercel.app/docs/getting-started/app-router-client-components) here.

Let's start by adding the dependency:

```bash
$ bun add next-intl
```

We'll also create a folder that will contain our localization files, which we'll call `locales/` in the root of the `ui-app/` project:

```bash
$ mkdir messages
```

This allows us to set up some text for our first languages. Create an English locale, `messages/en.json`, with the following:

```json
{
  "home": {
    "intro": "Welcome!"
  }
}
```

And also a French locale, in `messages/fr.json`:

```json
{
  "home": {
    "intro": "Bienvenue!"
  }
}
```

To make this a bit nicer to work with, we'll also [add typesafety](https://next-intl-docs.vercel.app/docs/workflows/typescript) by letting TypeScript know what keys we support in our localization function. Create a `ui-app/global.d.ts` file with the following:

```typescript
// Use type safe message keys with `next-intl`, based on the contents/keys of
// our default english locale.
type Messages = typeof import("./messages/en.json");
declare interface IntlMessages extends Messages {}
```

This ensures that if we misspell a key, or even remove one later on, we will be highlighted of the incorrect usage by TypeScript.

We can now set up a route using the App Router to pick up our locale. We'll want the locale to be part of our URL as a prefix on all routes, so that we can pick it up as a [dynamic segment](https://nextjs.org/docs/app/building-your-application/routing/defining-routes#creating-routes) and use it to load the correct localization file.

First we will create a folder where our localized pages will live in, and also clean up the default files that Next.js created for us:

```bash
$ mkdir "src/app/[locale]"
$ rm src/app/page.tsx src/app/layout.tsx
```

Let's create a simply page in here at `src/app/[locale]/page.tsx`, and get our welcome text from the localization file:

```typescript
"use client";

import { useTranslations } from "next-intl";

export default function Home() {
  const t = useTranslations("home");

  return (
    <div className="grid place-content-center content-center h-screen">
      <h1 className="text-6xl">{t("intro")}</h1>
      <div className="grid gap-4 grid-cols-2">
        <a href="/fr">Go to fr</a>
        <a href="/en">Go to en</a>
      </div>
    </div>
  );
}
```

We'll need to mark the component as `'use client'` for now, while [next-intl is working on server-side support](https://next-intl-docs.vercel.app/docs/getting-started/app-router-server-components).

Since we removed existing layout file, we need to define a new one that also handles setting up our localization at the root of our components. We'll create a `src/app/[locale]/layout.tsx` file with the following:

```typescript
import "../globals.css";
import type { Metadata } from "next";
import { Inter } from "next/font/google";
import { NextIntlClientProvider } from "next-intl";
import { notFound } from "next/navigation";
import { ReactNode } from 'react';
import { promises as fs } from 'fs';

export const metadata: Metadata = {
  title: "Hello, World!",
  description: "Ready to set things up",
}

const inter = Inter({ subsets: ["latin"] });

/**
 * Set up all supported locales as static parameters, which ensures we generate
 * static pages for each possible value of `locale`.
 */
export async function generateStaticParams() {
  // Construct an array of all supported locales based on the files in the `messages/` directory.
  const localeFiles = await fs.readdir(`${process.cwd()}/messages`);
  return localeFiles.map((f) => ({locale: f.replace(/\.json$/, "")}));
}

/**
 * Load the contents of a given locale's messages file.
 */
async function messagesContent(locale: string) {
  try {
    return (await import(`../../../messages/${locale}.json`)).default;
  } catch (error) {
    console.error("Something went wrong", error);
    notFound();
  }
}

type Props = {
  children: ReactNode;
  params: {
    locale: string;
  };
};

export default async function Layout({ children, params: { locale } }: Props) {
  const messages = await messagesContent(locale);
  return (
    <html lang={locale}>
      <body className={inter.className}>
        <NextIntlClientProvider locale={locale} messages={messages}>
          {children}
        </NextIntlClientProvider>
      </body>
    </html>
  );
}
```

To avoid needing to maintain a hardcoded list of locales, we dynamically find all the locales defined in our `messages/` folder during build time, and construct a list of supported locales from this. We then pass the contents of this into `NextIntlClientProvider`.

We could imagine later on, once our translation file becomes massive, that we split up `en.json` into smaller segments such as `en/home.json`, and load these parts specifically in a `app/[locale]/home/layout.tsx` file. For now though, we'll keep it simple.

As the final piece of this puzzle, we need a way to let Next.js know where it should route to by default, since we removed the default root pages.

We unfortunately cannot use [middleware's](https://next-intl-docs.vercel.app/docs/routing/middleware) when statically exporting our site, so we will instead redirect the user upon loading the page. Create a `src/app/page.tsx` file with the following:

```typescript
import { redirect } from "next/navigation";

export default function Root() {
  redirect("/en");
}
```

Along with a root layout file at `src/app/layout.tsx`:

```typescript
import { ReactNode } from "react";

type Props = {
  children: ReactNode;
};

export default function Layout({ children }: Props) {
  return children;
}
```

You should now have a structure that looks like this:

```bash
.
├── README.md
├── bun.lockb
├── global.d.ts
├── messages
│   └── en.json
│   └── fr.json
├── next.config.js
├── package.json
├── postcss.config.js
├── public
│   └── favicon.ico
├── src
│   └── app
│       ├── [locale]
│       │   ├── layout.tsx
│       │   └── page.tsx
│       ├── favicon.ico
│       ├── globals.css
│       ├── layout.tsx
│       └── page.tsx
├── tailwind.config.ts
└── tsconfig.json
```

And that's it! We're now able to run our app and check it out in the browser:

```bash
$ bun run dev
```

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-ui-app-skeleton.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-ui-app-skeleton.thumbnail.png" loading="lazy" alt="Screenshot of our ui-app" title="Screenshot of our ui-app" width="60%" /></a>
</div>

It may not look like much, but we've implemented a lot of the core functionality we need to get started, such as static builds and localization.

## Leptos (Rust/WASM)

Leptos is one of the newer entries on the Rust/WASM scene. It has a radical focus on performance and scalability of your codebase, so that as your codebase grows your App doesn't just start to become slower which is a typical issue in React and VDOM-based frameworks.

Leptos should feel somewhat familiar, although it is more closely related to something like [Solid.js](https://www.solidjs.com/) which is based on Signals and not using a VDOM. Leptos has a good quick overview of features [here](https://github.com/leptos-rs/leptos#what-does-that-mean) and a nice [FAQ here](https://github.com/leptos-rs/leptos#faqs).

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
```

We'll immediately adjust our `Cargo.toml` file with the dependencies we'll need, as well as a few common WASM optimizations for our release builds:

```toml
[package]
name = "ui-internal"
version = "0.1.0"
edition = "2021"

# Define our supported locales.
[package.metadata.leptos-i18n]
default = "en"
locales = ["en"]
locales-dir = "./messages"

# Optimize for WASM binary size.
[profile.release]
opt-level = 'z'
lto = true
codegen-units = 1

[dependencies]
# Core leptos library.
leptos = { version = "0.5.1", features = ["csr", "nightly"] }
# Leptos Meta adds support for adjusting <head> from within components.
leptos_meta = { version = "0.5.1", features = ["csr", "nightly"] }
# Router and Route state management.
leptos_router = { version = "0.5.1", features = ["csr", "nightly"] }
# Leptos support for i18n localization.
leptos_i18n = { version = "0.2", features = ["csr", "nightly"] }
# Lightweight logging support.
log = "0.4"

# Common WASM libraries.
wasm-bindgen = { version = "0.2" }
console_log = { version = "1" }
console_error_panic_hook = { version = "0.1" }

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

mod app;

pub fn main() {
    // Register log and panich handlers.
    let _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    mount_to_body(|| {
        view! { <app::Layout /> }
    });
}
```

We'll also create a `src/app.rs` file with the following (we'll update this file later):

```rust
use leptos::*;

#[component]
pub fn Layout() -> impl IntoView {
    view! { <p>"Hello, world!"</p>}
}
```

We can now run our App using Trunk:

```bash
$ trunk serve --open
```

Voila, we've got a little Hello World Leptos app!

#### Setting up Tailwind CSS

Let's configure Tailwind CSS for our Leptos App. First, we need to tell Tailwind where to look for files that might contain our CSS classes. Create a `ui-internal/tailwind.config.ts` file with the following:

```typescript
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: {
    files: ["*.html", "./src/**/*.rs"],
  },
  theme: {
    extend: {},
  },
  plugins: [],
};
```

We also need to tell `trunk` to build Tailwind CSS as part of its build process. We can do this by creating a `ui-internal/Trunk.toml` file:

```toml
[[hooks]]
stage = "pre_build"
command = "sh"
command_arguments = [
  "-c",
  "bunx tailwindcss --input resources/input.css --output public/output.css",
]
```

This let's `trunk` know that before it builds our WASM App, it should run the `bunx tailwindcss ...` command, which will generate our Tailwind CSS file, which it puts into `public/output.css`.

Now, you might have noticed we also have an input file. Let's get that set up, along with a `resources/` folder:

```bash
$ mkdir ui-internal/resources
```

We'll then create our base Tailwind CSS file at `ui-internal/resources/input.css`, mimicing our Next.js setup:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;

:root {
  --foreground-rgb: 0, 0, 0;
  --background-start-rgb: 214, 219, 220;
  --background-end-rgb: 255, 255, 255;
}

@media (prefers-color-scheme: dark) {
  :root {
    --foreground-rgb: 255, 255, 255;
    --background-start-rgb: 0, 0, 0;
    --background-end-rgb: 0, 0, 0;
  }
}

body {
  color: rgb(var(--foreground-rgb));
  background: linear-gradient(
      to bottom,
      transparent,
      rgb(var(--background-end-rgb))
    )
    rgb(var(--background-start-rgb));
}

```

Final step, we need to pull in our Tailwind CSS file in our `index.html`. Update the contents to the following:

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <link data-trunk rel="rust" data-wasm-opt="z" />
    <link data-trunk rel="icon" type="image/ico" href="/public/favicon.ico" />
    <link data-trunk rel="css" href="/public/output.css" />
    <title>Hello, World!</title>
  </head>

  <body></body>
</html>
```

And that's it! We've now integrated Tailwind CSS into our Leptos App.

#### Setting up Localization

We're using [leptos_i18n](https://github.com/Baptistemontan/leptos_i18n) for localization in Leptos, which supports an API that's very close to the one we used in Next.js. We already pulled in the dependency when we updated our `Cargo.toml` file earlier, so let's get the rest of it set up.

We'll create a `ui-internal/messages/` folder where our locales will live:

```bash
$ mkdir ui-internal/messages
```

We'll define our first locale, English, in a `messages/en.json` file:

```json
{
  "home": {
    "intro": "Welcome!"
  }
}
```

And also a French locale, in a `messages/fr.json` file:

```json
{
  "home": {
    "intro": "Bienvenue!"
  }
}
```

[leptos_i18n](https://github.com/Baptistemontan/leptos_i18n#loading-the-locales) exposes a macro `load_locales!()` that looks for our configuration and generates code specific for our project that we can load in our App.

Let's update `src/main.rs`, and also pull in a new module `home` in anticipation of creating splitting our code out from the current `app.rs` file:

```rust
use leptos::*;

mod app;
mod home;

// Load our locales from the files defined in `Cargo.toml`.
leptos_i18n::load_locales!();

pub fn main() {
    // Register log and panich handlers.
    let _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    mount_to_body(|| {
        view! { <app::Layout /> }
    });
}
```

Let's create a `src/home.rs` in which will use our locales:

```rust
use crate::i18n::*;
use leptos::*;
use leptos_router::*;

#[component]
pub fn Page() -> impl IntoView {
    let i18n = use_i18n();

    view! {
        <div class="grid place-content-center content-center h-screen">
            <h1 class="text-6xl">{t!(i18n, home.intro)}</h1>
            <div class="grid gap-4 grid-cols-2">
                <A href="/fr">"Go to fr"</A>
                <A href="/en">"Go to en"</A>
            </div>
        </div>
    }
}
```

The magic here comes from the `crate::i18n::*` which got generated by `leptos_i18n::load_locales!()`, and the `use_i18n` hook that we now got access to. Very similar to our Next.js App, we then call the macro `t!` to get the correct translation for the current locale, given a JSON key.

We're not entirely done yet, we need to tell our Leptos App about the `I18nContext` at the root of our application. We also still need to add support for routing between locales.

Let's update `src/app.rs` to do this:

```rust
use crate::i18n::*;
use leptos::{logging::log, *};
use leptos_meta::*;
use leptos_router::*;

use crate::home;

const DEFAULT_LOCALE: &str = "en";

#[derive(Params, PartialEq, Clone, Debug)]
pub struct LayoutParams {
    locale: String,
}

#[component(transparent)]
fn LocalizedRoute<P, F, IV>(path: P, view: F) -> impl IntoView
where
    P: std::fmt::Display,
    F: Fn() -> IV + 'static,
    IV: IntoView,
{
    view! {
        <Route path=format!("/:locale{}", path) view=move || {
            // Extract the locale from the path.
            let i18n = use_i18n();
            let params = use_params::<LayoutParams>();
            let chosen_locale = move || params().map(|params| params.locale).unwrap_or(DEFAULT_LOCALE.to_string());

            create_effect(move |_| {
                // Figure out what the current locale is, and if it matches the chosen locale from path.
                let current_locale = i18n();
                let new_locale = match chosen_locale().as_str() {
                    "fr" => Locale::fr,
                    "en" => Locale::en,
                    _ => Locale::en,
                };
                // Update the locale if necessary.
                if current_locale != new_locale {
                    i18n(new_locale);
                }
            });

            view! {
                {view()}
            }
        }/>
    }
}

#[component]
pub fn Layout() -> impl IntoView {
    provide_meta_context();
    provide_i18n_context();

    view! {
        <Router>
            <main>
                <Routes>
                    <Route path="" view=move || view! { <Redirect path=format!("/{}", DEFAULT_LOCALE)/> }/>
                    <LocalizedRoute path="" view=move || view! { <home::Page/> }/>
                </Routes>
            </main>
        </Router>
    }
}
```

There's a lot to unpack here, so let's go through it step by step.

- In our `pub fn Layout() -> impl IntoView` component we set up a normal `Router` component, which will handle routing for us.
- We introduce a special route, `LocalizedRoute`, which handles detecting our locale and switching the active locale if the path changes.
- In our `fn LocalizedRoute` function we wrap the normal `Route` component, and inject a bit of logic before returning the `view` that was otherwise passed into our `LocalizedRoute`.

The last part is the most interesting, so let's break down what we are doing inside the `Route` we are setting up for `LocalizedRoute`.

First we get the current parameters, which we know will contain a `locale` key:

```rust
// Extract the locale from the path.
let i18n = use_i18n();
let params = use_params::<LayoutParams>();
let chosen_locale = move || params().map(|params| params.locale).unwrap_or(DEFAULT_LOCALE.to_string());
```

We then create an effect that will run every time the parameters change, which will be every time the path changes:

```rust
create_effect(move |_| {
    // Figure out what the current locale is, and if it matches the chosen locale from path.
    let current_locale = i18n();
    let new_locale = match chosen_locale().as_str() {
        "fr" => Locale::fr,
        "en" => Locale::en,
        _ => Locale::en,
    };
    // Update the locale if necessary.
    if current_locale != new_locale {
        i18n(new_locale);
    }
});
```

The thing that makes our effect rerun is our usage of `i18n()` which subscribes us to the signal, and thus reruns the effect every time the locale changes.


You should now have a structure that looks like this:

```bash
.
├── Cargo.lock
├── Cargo.toml
├── Trunk.toml
├── index.html
├── messages
│   ├── en.json
│   └── fr.json
├── public
│   └── favicon.ico
├── resources
│   └── input.css
├── src
│   ├── app.rs
│   ├── home.rs
│   └── main.rs
├── tailwind.config.ts
```

And that's it! We're now able to run our app and check it out in the browser:

```bash
$ trunk serve --open
```

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-ui-internal-skeleton.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-ui-internal-skeleton.thumbnail.png" loading="lazy" alt="Screenshot of our ui-internal" title="Screenshot of our ui-internal" width="60%" /></a>
</div>

Again, it may not look like much, but we've implemented a lot of the core functionality we need to get started!

## Automating Deployments via CDK

- Build artifact
- Publish to S3 + CloudFront
- Configure API route proxy on CloudFront already?


## Bonus: End-to-End Tests

- Simple Playwright setup in both ui-app and ui-internal
- Update our deployments to run these




## Bonus: DevEx Improvements

There are a few Editor improvements [that are recommended](https://leptos-rs.github.io/leptos/appendix_dx.html) for working with Leptos and Tailwind CSS if you are using VS Code.

Add to your settings:

```json
{
  "rust-analyzer.procMacro.ignored": {
    "leptos_macro": ["server", "component"]
  },
  "emmet.includeLanguages": {
    "rust": "html",
    "*.rs": "html"
  },
  "tailwindCSS.includeLanguages": {
    "rust": "html",
    "*.rs": "html"
  },
  "files.associations": {
    "*.rs": "rust"
  },
  "editor.quickSuggestions": {
    "other": "on",
    "comments": "on",
    "strings": true
  },
  "css.validate": false
}
```

Another nice tool is [leptosfmt](https://github.com/bram209/leptosfmt), which helps keep our Leptos View macro code nicely formatted.

You can install it via:

```bash
$ cargo install leptosfmt
```

And then add this to your settings:

```json
{
  "rust-analyzer.rustfmt.overrideCommand": ["leptosfmt", "--stdin", "--rustfmt"]
}
```

## Next Steps

Next up is to set up our Federated GraphQL API! Follow along in Part 4 of the series (will be posted soon).
