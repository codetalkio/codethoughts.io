---
title: "The Stack Part 3: Building a Frontend"
tags: rust, typescript, wasm, nextjs, leptos, aws, cloud, infrastructure, cdk, ci
date: 2023-10-16
---

In [the last post](/posts/2023-10-07-the-stack-part-2.html) we set up our deployment, fully automated on merge to our `main` branch. In this post we will be building our UI (Frontend) applications. See the full overview of posts [here](/posts/2023-01-29-the-stack.html#what-will-we-be-covering).

At the end of this post we will have:

- A [Next.js](https://nextjs.org/) Frontend app with support for localization, using Tailwind CSS.
- A [Leptos](https://github.com/leptos-rs/leptos) Rust/WASM Frontend app with support for localization, using Tailwind CSS.
- Automatic deployment of our Apps AWS using CDK, statically hosted using S3 + CloudFront.

We are essentially hedging our bets by building both a JavaScript-based Frontend, which is the safe bet, and a Rust/WASM-based Frontend, which is the future bet. We will be using the same GraphQL API for both, so we can easily switch between them.

There is quite a lot to cover. My recommendation is to clone down the Part 3 branch in the [GitHub repository](https://github.com/codetalkio/the-stack/tree/part-3-frontend) and use this post as an explanation of what is set up.

<div></div><!--more-->

- [Prelude: Static Site Generation](#prelude-static-site-generation)
- [Next.js](#next.js)
    - [Setting up our Next.js App](#setting-up-our-next.js-app)
    - [Building Static Files](#building-static-files)
    - [Setting up Localization](#setting-up-localization)
- [Leptos (Rust/WASM)](#leptos-rustwasm)
    - [Setting up our Leptos App](#setting-up-our-leptos-app)
    - [Setting up Tailwind CSS](#setting-up-tailwind-css)
    - [Setting up Localization](#setting-up-localization-1)
- [Bonus: End-to-End Tests](#bonus-end-to-end-tests)
- [Bonus: DevEx Improvements](#bonus-devex-improvements)
- [Automating Deployments via CDK](#automating-deployments-via-cdk)
    - [Building artifacts in CI](#building-artifacts-in-ci)
    - [Deploying to S3 + CloudFront](#deploying-to-s3-cloudfront)
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

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(1).sh

While [bun unfortunately doesn't fully support the Next.js App Router yet](https://bun.sh/guides/ecosystem/nextjs) we will still rely on it for installing dependencies and being our general go-to tool for running anything JS related.

#### Setting up our Next.js App

Let's get our Next.js app set up, which we will call `ui-app`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(2).sh

This gets us quite far, we now have an App we can run, Tailwind CSS is already set up, and we got a lot of the structure set up for us:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(3).sh

Voila, we've got a little Hello World Next.js app!

#### Building Static Files

We need to do just one small change to our Next.js setup to make it output static files for us. We'll do this by adding `output: "export"` to our `next.config.js` file at the root of `ui-app/`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=next.config.js

We also enabled `trailingSlash` ([docs here](https://nextjs.org/docs/pages/api-reference/next-config-js/trailingSlash)) to make Next.js work nicely with CloudFront.

This tells Next.js that we want to to [Statically Export](https://nextjs.org/docs/app/building-your-application/deploying/static-exports) our files, which will generate an HTML file per route, which allows each route to serve the minimal content it needs, enabling faster page loads, instead of the traditional SPA approach of serving one large file upfront.

We get the best of both worlds here, as we still get the reduced bundle sizes typical of SSR, but can retain the static file advantage of SPAs.

#### Setting up Localization

As our customer-base grows, we will inevitably run into the need to localization. To do this, we will restructure our App with basic support for this, as well as bring in a dependency to help us with this, namely [next-intl](https://www.npmjs.com/package/next-intl). Vercel also has some good documentation on how to [Get Started](https://next-intl-docs.vercel.app/docs/getting-started/app-router-client-components) here.

Let's start by adding the dependency:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(5).sh

We'll also create a folder that will contain our localization files, which we'll call `messages/` in the root of the `ui-app/` project:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(6).sh

This allows us to set up some text for our first languages. Create an English locale, `messages/en.json`, with the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=messages%5Cen.json

And also a French locale, in `messages/fr.json`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=messages%5Cfr.json

To make this a bit nicer to work with, we'll also [add typesafety](https://next-intl-docs.vercel.app/docs/workflows/typescript) by letting TypeScript know what keys we support in our localization function. Create a `ui-app/global.d.ts` file with the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-app%5Cglobal.d.ts

This ensures that if we misspell a key, or even remove one later on, we will be highlighted of the incorrect usage by TypeScript.

We can now set up a route using the App Router to pick up our locale. We'll want the locale to be part of our URL as a prefix on all routes, so that we can pick it up as a [dynamic segment](https://nextjs.org/docs/app/building-your-application/routing/defining-routes#creating-routes) and use it to load the correct localization file.

First we will create a folder where our localized pages will live in, and also clean up the default files that Next.js created for us:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(10).sh

Let's create a simply page in here at `src/app/[locale]/page.tsx`, and get our welcome text from the localization file:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=src%5Capp%5C%5Blocale%5D%5Cpage.tsx

We'll need to mark the component as `'use client'` for now, while [next-intl is working on server-side support](https://next-intl-docs.vercel.app/docs/getting-started/app-router-server-components).

Since we removed existing layout file, we need to define a new one that also handles setting up our localization at the root of our components. We'll create a `src/app/[locale]/layout.tsx` file with the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=src%5Capp%5C%5Blocale%5D%5Clayout.tsx

To avoid needing to maintain a hardcoded list of locales, we dynamically find all the locales defined in our `messages/` folder during build time, and construct a list of supported locales from this. We then pass the contents of this into `NextIntlClientProvider`.

We could imagine later on, once our translation file becomes massive, that we split up `en.json` into smaller segments such as `en/home.json`, and load these parts specifically in a `app/[locale]/home/layout.tsx` file. For now though, we'll keep it simple.

As the final piece of this puzzle, we need a way to let Next.js know where it should route to by default, since we removed the default root pages.

We unfortunately cannot use [middlewares](https://next-intl-docs.vercel.app/docs/routing/middleware) when statically exporting our site, so we will instead redirect the user upon loading the page. Create a `src/app/page.tsx` file with the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=src%5Capp%5Cpage.tsx

Along with a root layout file at `src/app/layout.tsx`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=14.ts

You should now have a structure that looks like this:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(15).sh

And that's it! We're now able to run our app and check it out in the browser:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(16).sh

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-ui-app-skeleton.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-ui-app-skeleton.thumbnail.png" loading="lazy" alt="Screenshot of our ui-app" title="Screenshot of our ui-app" width="60%" /></a>
</div>

It may not look like much, but we've implemented a lot of the core functionality we need to get started, such as static builds and localization.

As the final step we will add our commands to just, [extending our existing justfile](/posts/2023-10-07-the-stack-part-2.html#bonus-just):

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile

We'll also set up a new command for running our development server:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile%20(Setting%20up%20Localization).makefile

## Leptos (Rust/WASM)

Leptos is one of the newer entries on the Rust/WASM scene. It has a radical focus on performance and scalability of your codebase, so that as your codebase grows your App doesn't just start to become slower which is a typical issue in React and VDOM-based frameworks.

Leptos should feel somewhat familiar, although it is more closely related to something like [Solid.js](https://www.solidjs.com/) which is based on Signals and not using a VDOM. Leptos has a good quick overview of features [here](https://github.com/leptos-rs/leptos#what-does-that-mean) and a nice [FAQ here](https://github.com/leptos-rs/leptos#faqs).

#### Setting up our Leptos App

We will be using Trunk for developing and building our Leptos App. Trunk is a great tool for developing Rust/WASM Apps, and is very similar to Bun (in a sense) in that it is a wrapper around the underlying tools. Let's install it first:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(19).sh

We can then set up our project, which we'll call `ui-internal`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(20).sh

We'll immediately adjust our `Cargo.toml` file with the dependencies we'll need, as well as a few common WASM optimizations for our release builds:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5CCargo.toml

And finally, we'll use Rust Nightly to develop our App, which gives us a few better ergonomics:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(22).sh

Let's create a quick `index.html` file in the root of the `ui-internal/` folder, just to get started:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Cindex.html

And replace the contents of our `src/main.rs`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Cmain.rs

We'll also create a `src/app.rs` file with the following (we'll update this file later):

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Capp.rs

We can now run our App using Trunk:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(26).sh

Voila, we've got a little Hello World Leptos app!

#### Setting up Tailwind CSS

Let's configure Tailwind CSS for our Leptos App. First, we need to tell Tailwind where to look for files that might contain our CSS classes. Create a `ui-internal/tailwind.config.ts` file with the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Ctailwind.config.ts

We also need to tell `trunk` to build Tailwind CSS as part of its build process. We can do this by creating a `ui-internal/Trunk.toml` file:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5CTrunk.toml

This let's `trunk` know that before it builds our WASM App, it should run the `bunx tailwindcss ...` command, which will generate our Tailwind CSS file, which it puts into `public/output.css`.

Now, you might have noticed we also have an input file. Let's get that set up, along with a `resources/` folder:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(29).sh

We'll then create our base Tailwind CSS file at `ui-internal/resources/input.css`, mimicing our Next.js setup:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Cresources%5Cinput.css

Final step, we need to pull in our Tailwind CSS file in our `index.html`. Update the contents to the following:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Cindex.html%20(Setting%20up%20Tailwind%20CSS).html

And that's it! We've now integrated Tailwind CSS into our Leptos App.

#### Setting up Localization

We're using [leptos_i18n](https://github.com/Baptistemontan/leptos_i18n) for localization in Leptos, which supports an API that's very close to the one we used in Next.js. We already pulled in the dependency when we updated our `Cargo.toml` file earlier, so let's get the rest of it set up.

We'll create a `ui-internal/messages/` folder where our locales will live:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(32).sh

We'll define our first locale, English, in a `messages/en.json` file:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Cmessages%5Cen.json

And also a French locale, in a `messages/fr.json` file:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Cmessages%5Cfr.json

[leptos_i18n](https://github.com/Baptistemontan/leptos_i18n#loading-the-locales) exposes a macro `load_locales!()` that looks for our configuration and generates code specific for our project that we can load in our App.

Let's update `src/main.rs`, and also pull in a new module `home` in anticipation of creating splitting our code out from the current `app.rs` file:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Cmain.rs%20(Setting%20up%20Localization).rs

Let's create a `src/home.rs` in which will use our locales:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Chome.rs

The magic here comes from the `crate::i18n::*` which got generated by `leptos_i18n::load_locales!()`, and the `use_i18n` hook that we now got access to. Very similar to our Next.js App, we then call the macro `t!` to get the correct translation for the current locale, given a JSON key.

We're not entirely done yet, we need to tell our Leptos App about the `I18nContext` at the root of our application. We also still need to add support for routing between locales.

Let's update `src/app.rs` to do this:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Capp.rs%20(Setting%20up%20Localization).rs

There's a lot to unpack here, so let's go through it step by step.

- In our `pub fn Layout() -> impl IntoView` component we set up a normal `Router` component, which will handle routing for us.
- We introduce a special route, `LocalizedRoute`, which handles detecting our locale and switching the active locale if the path changes.
- In our `fn LocalizedRoute` function we wrap the normal `Route` component, and inject a bit of logic before returning the `view` that was otherwise passed into our `LocalizedRoute`.

The last part is the most interesting, so let's break down what we are doing inside the `Route` we are setting up for `LocalizedRoute`.

First we get the current parameters, which we know will contain a `locale` key:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Capp.rs%20(Setting%20up%20Localization).rs

We then create an effect that will run every time the parameters change, which will be every time the path changes:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=ui-internal%5Csrc%5Capp.rs%20(Setting%20up%20Localization).rs

The thing that makes our effect rerun is our usage of `i18n()` which subscribes us to the signal, and thus reruns the effect every time the locale changes.


You should now have a structure that looks like this:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(40).sh

And that's it! We're now able to run our app and check it out in the browser:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(41).sh

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-ui-internal-skeleton.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-ui-internal-skeleton.thumbnail.png" loading="lazy" alt="Screenshot of our ui-internal" title="Screenshot of our ui-internal" width="60%" /></a>
</div>

Again, it may not look like much, but we've implemented a lot of the core functionality we need to get started!

As the final step we will add our commands to just, [extending our existing justfile](/posts/2023-10-07-the-stack-part-2.html#bonus-just):

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile%20(Setting%20up%20Localization).makefile

## Bonus: End-to-End Tests

End-to-End tests are a great way to ensure that our App is working as expected, and that we don't accidentally break something when we make changes. We'll use [Playwright](https://playwright.dev/) for this, which is a great tool for writing End-to-End tests.

We want three different test suites to cover:

- **ui-app**: Test our Next.js App.
- **ui-internal**: Test our Leptos App.
- **deployment**.: Test our deployed App to verify it is working as expected.

Let's start by setting up our folder structure. Many of our configuration files will be the same across all three test suites, let's create an `end2end` folder for our projects:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(43).sh

We intentionally make a distinction between `end2end/` and unit/integration tests which will live in `tests/`. These have very different requirements for how to run them, and we often want to run them at different times.

Before we can run anything, we will need a couple of other files to set up Playwright as well as support for TypeScript.

Let's create a `tsconfig.json` for all for all three projects (`ui-app`, `ui-internal`, and `deployment`). We'll place it at `<project>/end2end/tsconfig.json`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=%3Cproject%3E%5Cend2end%5Ctsconfig.json

Now, let's configure Playwright for all for all three projects (`ui-app`, `ui-internal`, and `deployment`). We'll place it at `<project>/end2end/playwright.config.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=%3Cproject%3E%5Cend2end%5Cplaywright.config.ts

There are some minor adjustments we want to do in the above configuration for each project:

- **ui-app**: Set the `SERVER` variable to `http://localhost:3000` and command to `just dev ui-app`.
- **ui-internal**: Set the `SERVER` variable to `http://localhost:8080` and command to `just dev ui-internal`.
- **deployment**: Remove the `webServer` block, and set the `SERVER` variable to `http://${process.env.DOMAIN}`.


And a `package.json` in `<project>/end2end/package.json`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=%3Cproject%3E%5Cend2end%5Cpackage.json

We are now ready to add our first test! Since we have just added localization, let's make sure that it works and doesn't regress.

For all three projects `ui-app`, `deployment`, and `ui-internal` we'll create a test in `<project>/end2end/tests/localization.spec.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=%3Cproject%3E%5Cend2end%5Ctests%5Clocalization.spec.ts

This same test works for both apps since we've set them up with the same functionality.

Let's try and run them:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(48).sh

And for `deployment` we can test it locally by starting up `just dev ui-app` in another terminal, and then running:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(49).sh

NOTE: You might want to add the following to your `.gitignore`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=50.name%3D.gitignore

And that's it! We've now got an easy way to run End-to-End tests. Let's do our final step and add this to our `justfile`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile%20(Bonus%3A%20End-to-End%20Tests).makefile

And we'll also update our `_setup-project` commands to setup the Playwright dependencies:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile%20(Bonus%3A%20End-to-End%20Tests).makefile

## Bonus: DevEx Improvements

There are a few Editor improvements [that are recommended](https://leptos-rs.github.io/leptos/appendix_dx.html) for working with Leptos and Tailwind CSS if you are using VS Code.

Add to your settings:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.vscode%5Csettings.json

Another nice tool is [leptosfmt](https://github.com/bram209/leptosfmt), which helps keep our Leptos View macro code nicely formatted.

You can install it via:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=terminal%20(54).sh

And then add this to your settings:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.vscode%5Csettings.json%20(Bonus%3A%20DevEx%20Improvements).json

## Automating Deployments via CDK

Since we are now generating artifacts that we want to deploy, from multiple projects, we need to restructure our existing deployment pipeline slightly.

We still want to retain staggered deployments, but we need a bit of coordination to make sure we have all relevant artifacts before we deploy.

Our new flow will look like this:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-updated-flow.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-updated-flow.thumbnail.png" loading="lazy" alt="Updated deployment pipeline" title="Updated deployment pipeline" width="100%" /></a>
</div>

#### Building artifacts in CI

In this part we will be doing the following:

- Extend our existing `cd-deploy.yml` workflow to build artifacts for `ui-app` and `ui-internal` via a reuseable workflow.
- Extend our existing `wf-deploy.yml` workflow to download artifacts so it can use it during deployments.
- Set up a reuseable workflow, `wf-build.yml`, that will build our artifacts.
- Set up a reuseable workflows for both `ui-app` and `ui-internal` that will do the actual building.


Let's start with our `wf-build-ui-app.yml` workflow:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.github%5Cworkflows%5Cwf-build-ui-app.yml

And our `wf-build-ui-internal.yml` workflow:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.github%5Cworkflows%5Cwf-build-ui-internal.yml

Both of these workflows take two optional arguments:

- `release`: Whether or not to build a release build.
- `upload-artifact`: Whether or not to upload the artifact to GitHub.

This means we can easily reuse these builds from our CI workflows. Once our jobs are building, we'll see these artifacts in the **Summary** view of our workflow, which will look something like this:

<div style="text-align:center;">
<a href="/resources/images/the-stack-part-3-artifacts.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/the-stack-part-3-artifacts.thumbnail.png" loading="lazy" alt="Deployment artifacts" title="Deployment artifacts" width="70%" /></a>
</div>

With these in place we can now stitch them together in a `wf-build.yml`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.github%5Cworkflows%5Cwf-build.yml

Not much going on here, we are simply calling our previously defined reuseable workflows.

We can now update our `cd-deploy.yml` workflow to call our new `wf-build.yml` workflow. To do this, we extend the existing file by adding a `build-artifacts` job as well as mark our `stage-1` job as `needs: [build-artifacts]`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.github%5Cworkflows%5Ccd-deploy.yml

The final change we need to make is to make our `wf-deploy.yml` workflow download the artifacts we just built:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=.github%5Cworkflows%5Cwf-deploy.yml

The new additions here are the steps:

- `extractions/setup-just@v1`: Make `just` available in our workflow.
- `Setup artifact directory`: Creating our artifacts folder.
- `actions/download-artifact@v3y`: Download all uploaded assets into this folder.
- `Display structure of downloaded files`: Very helpful for any debugging.
- `Validate artifacts`: Make sure we have all the artifacts we need.

The `deploy-validate-artifacts` command is defined in our `justfile`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=justfile%20(Building%20artifacts%20in%20CI).makefile


#### Deploying to S3 + CloudFront

If we remember our three groupings, we see that `ui-app` and `ui-internal` would fit into `Services`:

- `Global`: "Global" (often `us-east-1`) specific things such as ACM Certificates for CloudFront, and we'll also put Hosted Zones here
- `Cloud`: Region specific infrequently changing things such as VPC, Region-specific Certificates, etc
- `Platform`: DynamoDB, Cache, SQS
- `Services`: Lambdas, API Gateway, etc

##### Preparing for CloudFront

We'd like our CloudFront distribution to use its own domain name and HTTPS, and to do this requires a bit of extra work on our end, since CloudFront needs our ACM Certificate to live in `us-east-1` specifically.

This means that it will fit into our `Global` stack. Cross-stack references in CDK/CloudFormation is a bit finicky, and we generally want to avoid relying on `Exports` which are incredibly frustrating to work with and often gets you into sticky situations where you have to carefully destroy or update your stacks in a certain order. Once an export is used, it cannot change, leading to very tight coupling between stacks.

<div class="callout">
  <div class="callout-bulb">💡</div>
  Some examples of issues/annoyances and workarounds with `Exports`: [Here](https://github.com/aws/aws-cdk/issues/17741), [here](https://chariotsolutions.com/blog/post/limiting-cross-stack-references-in-cdk/), [here](https://github.com/aws/aws-cdk/issues/5304), and [here](https://www.endoflineblog.com/cdk-tips-03-how-to-unblock-cross-stack-references).
</div>

Instead, we will rely on the approach outlined in this nice article from AWS on how to [Read parameters across AWS Regions with AWS CloudFormation custom resources](https://aws.amazon.com/blogs/infrastructure-and-automation/read-parameters-across-aws-regions-with-aws-cloudformation-custom-resources/).

We will essentially:

- Store the Certificate ARN in the SSM Parameter Store in `us-east-1`.
- Set up a new construct via `AwsCustomResource` and `AwsSdkCall` that can read parameters from a specific region.
- Use this construct in our `Services` stack to read the Certificate ARN from `us-east-1`.

Let's set up our new Certificate first. We'll adjust the existing `GlobalStack` slightly in `bin/deployment.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Cbin%5Cdeployment.ts

We've introduced `GLOBAL_CERTIFICATE_SSM` which will be how we share the name of the parameter across stacks, and `certificateArnSsm` as a property to our `GlobalStack`.

Let's set up the certificate before we stitch it into our `GlobalStack`. We'll create a new file `lib/global/certificate.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Clib%5Cglobal%5Ccertificate.ts

The last step in the stack stores the `certificateArn` in the SSM Parameter Store.

Finally, we adjust `lib/global/stack.ts` to now look like:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Clib%5Cglobal%5Cstack.ts

Instead of passing the Hosted Zone into the certificate stack, we explicitly mark the certificate as dependent on the domain stack to ensure the hosted zone exists before we try to access it. Again, avoiding exports.

Normally SSM doesn't take the region as a parameter, so to access the parameter from `us-east-1` we'll set up a new construct in `lib/services/ssm-global.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Clib%5Cservices%5Cssm-global.ts

We now have everything we need to create our services.

##### Services

Now we are ready to get our `Services` stack set up!

All files will live in the `deployment/` folder. We'll first adjust our `bin/deployment.ts`, adding our `Services` stack. Append the following at the end:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Cbin%5Cdeployment.ts%20(Services).ts

And our `ServicesStack` is defined in `lib/services/stack.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Clib%5Cservices%5Cstack.ts

In here we deploy both `ui-app` and `ui-internal` the same way, but do some minor adjustments to the props we pass on to the stack to ensure it gets the right assets and also the right domain.

This brings us to our final part, which is the most lengthy, our `lib/services/s3-website.ts`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Clib%5Cservices%5Cs3-website.ts

And our Lambda@Edge function to rewrite urls is defined in `edge-functions/rewrite-urls.js`:

https://gist.github.com/Tehnix/1bba1d79dd4c917a901e93bd588be471.js?file=deployments%5Cedge-functions%5Crewrite-urls.js


There is quite a bit going on here. A rough overview of what is happening:

- We create an S3 Bucket, which will host our assets.
- The S3 Bucket will be configured to with encryption and to block public read access.
- We create a CloudFront distribution, which will serve our assets.
- The CloudFront distribution will also redirect HTTP to HTTPS, use our domain name and certificate, as well as support HTTP 2 and 3.
- If enabled via `rewriteUrls`, we will also set up a Lambda@Edge function that will rewrite URLs to `/folder/` -> `/folder/index.html`. This is necessary to support the way Next.js generates its files.

And that's it! Your `ui-app` will now live at the root of your domain, e.g. `app.example.com`, and `ui-internal` will live at the subdomain `internal` e.g. `internal.app.example.com`.

## Next Steps

Next up is to set up our Federated GraphQL API! Follow along in Part 4 of the series (will be posted soon).
