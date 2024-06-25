---
title: "Using Capacitor Plugins from Rust/WASM"
tags: rust, wasm, leptos, mobile
date: 2024-06-24
---

This is a part of the post [Mobile: A different way using Rust?](/posts/2024-06-25-mobile-a-different-way.html).

Capacitor is normally used in combination with JavaScript projects, so there is little help available if you want to use it in a Rust/WASM project. Worry not! That's exactly what we'll take a look at in this post.

Since Capacitor is a JS framework there will be a little extra work involved in interacting with any plugins, but it’s honestly not a lot we need to do. Let’s take a quick tour through what I’ve set up in the example repository [https://github.com/Tehnix/template-mobile-wasm](https://github.com/Tehnix/template-mobile-wasm)[.](https://github.com/Tehnix/playground-mobile-wasm?tab=readme-ov-file)

<div></div><!--more-->

We've set up four crates:

- `appy`: Our Leptos App, Capacitor, and the XCode project
- `capacitor-rs`: Bridging code between the Capacitor JS library and our Rust code
- `shared`: Our shared code that we might use in `appy`, and also want to expose in Swift to use in our Widgets or watchOS App
- `mobile`: Where we will generate the Swift bindings from via UniFFI, reexporting everything from `shared` that’s made available to UniFFI via the macros

In this post we'll focus on the `capacitor-rs` and `appy` crates, which contain our Capacitor bridge and our app where we'll use it respectively.

First, let’s create a new library where we’ll keep the Capacitor bridge related code in:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=terminal%20(1).sh

We’ll also initialize an empty TypeScript project using bun which we’ll use to install the Capacitor dependencies and bundle our TypeScript files:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=terminal%20(2).sh

We’ll then create a few new folders to host our code in (assuming you’re in the `./capacitor-rs` directory):

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=terminal%20(3).sh

Let’s set up our `capacitor-rs/src/lib.rs` to expose the plugins folder:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=src%5Clib.rs

And finally, let’s update our dependencies of the Rust project in `./capacitor-rs/Cargo.toml`:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=capacitor-rs%5CCargo.toml

For this example we’ll set up the [Haptics](https://capacitorjs.com/docs/apis/haptics) plugin, which will allow us to provide tactile feedback from our App in response to user interactions.

Our approach will be:

1. Install the capacitor plugin in both the `./appy` and `./capacitor` projects
    1. `./appy`: Capacitor needs access to the plugins when packaging our App
    2. `./capacitor-rs`: We’ll need access to the libraries to be able to bundle them with bun into a single file [since `wasm_bindgen` does not support imports](https://rustwasm.github.io/wasm-bindgen/reference/js-snippets.html?highlight=imports#caveats)
2. Add a TypeScript file that imports the plugin and exports functions to interact with it
3. Bundle the TypeScript file using bun to create a single file without any imports in it
4. Add a Rust file that uses the `wasm_bindgen` proc macro to create a Rust interface for the TypeScript functions we’ve defined

Let’s add the TypeScript side of things in `./js/haptics.ts` which quite simply just wraps and exposes the Capacitor functionality:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=capacitor-rs%5Cjs%5Chaptics.ts

We then bundle this with bun:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=7.tsx

Which gives us a corresponding bundled file in `js-dist/haptics.js` that we can use.

As the final step, we’ll bridge this into our Rust code by setting up `src/plugins/haptics.rs`:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=capacitor-rs%5Csrc%5Cplugins%5Chaptics.rs

Let’s break down what’s going on here:

- We mirror some of the types from the Capacitor plugin library into Rust to `ImpactOptions`, `ImpactStyle` (unfortunately `wasm_bindgen` doesn’t support generating these for us, but it’s more or less a copy-paste of the TypeScript code)
- We point to our generated JavaScript file `#[wasm_bindgen(module = "/js-dist/haptics.js")]` which will ensure it gets included automatically
- We setup the type signatures matching each of the TypeScript functions we defined and that we want to include

We also need to add a `src/plugins/mod.rs` file to expose our new plugin:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=capacitor-rs%5Csrc%5Cplugins%5Cmod.rs

We’re now ready to use it in our App by adding the new `capacitor-rs` crate to our dependencies in our WASM app:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=capacitor-rs%5CCargo.toml%20(title%3A%20%22Using%20Capacitor%20Plugins%20from%20Rust%5CWASM%22%0Atags%3A%20rust%2C%20wasm%2C%20leptos%2C%20mobile%0Adate%3A%202024-06-24).toml

And then using it like you like a normal Rust function:

https://gist.github.com/Tehnix/3d5c588437210239743244db0b34aa0c.js?file=example-component.rs

And that’s it!

Since I can’t exactly screenshot a haptic vibration, here’s an example where we use the [Capacitor](https://capacitorjs.com/docs/basics/utilities) utility function to determine which platform we are on, set up in the same way:

<div class="clear two-images">
  <a href="/resources/images/using-capacitor-plugins-from-rust-wasm-iphone.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/using-capacitor-plugins-from-rust-wasm-iphone.thumbnail.png" loading="lazy" alt="Screenshot of Capacitor running on iPhone" title="Screenshot of Capacitor running on iPhone" style="margin-right: 1%; width: 49%;" /></a>
  <a href="/resources/images/using-capacitor-plugins-from-rust-wasm-web.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/using-capacitor-plugins-from-rust-wasm-web.thumbnail.png" loading="lazy" alt="Screenshot of Capacitor running in Web" title="Screenshot of Capacitor running in Web" style="margin-left: 1%; width: 49%;" /></a>
</div>
<div class="clear"></div>
