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

```bash
cargo new --lib capacitor-rs
```

We’ll also initialize an empty TypeScript project using bun which we’ll use to install the Capacitor dependencies and bundle our TypeScript files:

```bash
$ bun init
bun init helps you get started with a minimal project and tries to guess sensible defaults. Press ^C anytime to quit

package name (appy): capacitor-wrapper
entry point (index.ts): index.ts
...
```

We’ll then create a few new folders to host our code in (assuming you’re in the `./capacitor-rs` directory):

```bash
# We'll expose an interface for Capacitor from JS here.
mkdir js
# We'll keep our bundled JS files here.
mkdir js-dist
# We'll keep the corresponding Rust files here.
mkdir src/plugins
```

Let’s set up our `capacitor-rs/src/lib.rs` to expose the plugins folder:

```rust name=src/lib.rs
pub mod plugins;
```

And finally, let’s update our dependencies of the Rust project in `./capacitor-rs/Cargo.toml`:

```toml
[package]
name = "capacitor-rs"
version = "0.1.0"
edition = "2021"

[dependencies]
# Match Trunk's wasm-bindgen version for compatability.
wasm-bindgen = { version = "=0.2.87" }
# Used for working with async/promises in JS.
wasm-bindgen-futures = { version = "0.4" }
js-sys = { version = "0.3" }
```

For this example we’ll set up the [Haptics](https://capacitorjs.com/docs/apis/haptics) plugin, which will allow us to provide tactile feedback from our App in response to user interactions.

Our approach will be:

1. Install the capacitor plugin in both the `./appy` and `./capacitor` projects
    1. `./appy`: Capacitor needs access to the plugins when packaging our App
    2. `./capacitor-rs`: We’ll need access to the libraries to be able to bundle them with bun into a single file [since `wasm_bindgen` does not support imports](https://rustwasm.github.io/wasm-bindgen/reference/js-snippets.html?highlight=imports#caveats)
2. Add a TypeScript file that imports the plugin and exports functions to interact with it
3. Bundle the TypeScript file using bun to create a single file without any imports in it
4. Add a Rust file that uses the `wasm_bindgen` proc macro to create a Rust interface for the TypeScript functions we’ve defined

Let’s add the TypeScript side of things in `./js/haptics.ts` which quite simply just wraps and exposes the Capacitor functionality:

```tsx name=capacitor-rs/js/haptics.ts
/**
 * https://capacitorjs.com/docs/apis/haptics
 */
import type { ImpactOptions } from "@capacitor/haptics";
import { Haptics, ImpactStyle } from "@capacitor/haptics";

export const haptics_impact = async (options?: ImpactOptions | undefined) => {
  await Haptics.impact(options);
};

export const haptics_impact_medium = async () => {
  await Haptics.impact({ style: ImpactStyle.Medium });
};

export const haptics_impact_light = async () => {
  await Haptics.impact({ style: ImpactStyle.Light });
};

export const haptics_vibrate = async () => {
  await Haptics.vibrate();
};

export const haptics_selection_start = async () => {
  await Haptics.selectionStart();
};

export const haptics_selection_changed = async () => {
  await Haptics.selectionChanged();
};

export const haptics_selection_end = async () => {
  await Haptics.selectionEnd();
};
```

We then bundle this with bun:

```tsx
bun build --target browser --minify --outdir js-dist js/**
```

Which gives us a corresponding bundled file in `js-dist/haptics.js` that we can use.

As the final step, we’ll bridge this into our Rust code by setting up `src/plugins/haptics.rs`:

```rust name=capacitor-rs/src/plugins/haptics.rs
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct ImpactOptions {
    /// Impact Feedback Style
    ///
    /// The mass of the objects in the collision simulated by a [UIImpactFeedbackGenerator](https://developer.apple.com/documentation/uikit/uiimpactfeedbackstyle) object.
    ///
    /// @default ImpactStyle.Heavy
    /// @since 1.0.0
    pub style: ImpactStyle,
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub enum ImpactStyle {
    /// A collision between large, heavy user interface elements
    ///
    /// @since 1.0.0
    Heavy,
    /// A collision between moderately sized user interface elements
    ///
    /// @since 1.0.0
    Medium,
    /// A collision between small, light user interface elements
    ///
    /// @since 1.0.0
    Light,
}

impl ImpactStyle {
    pub fn as_str(&self) -> &'static str {
        match self {
            ImpactStyle::Heavy => "HEAVY",
            ImpactStyle::Medium => "MEDIUM",
            ImpactStyle::Light => "LIGHT",
        }
    }
}

#[wasm_bindgen(module = "/js-dist/haptics.js")]
extern "C" {
    // #[wasm_bindgen(typescript_type = "ImpactOptions")]
    // pub type ImpactOptions;

    /// Trigger a haptics "impact" feedback.
    #[wasm_bindgen]
    pub async fn haptics_impact(options: ImpactOptions);

    #[wasm_bindgen]
    /// Trigger a haptics medium "impact" feedback.
    pub async fn haptics_impact_medium();

    #[wasm_bindgen]
    /// Trigger a haptics light "impact" feedback.
    pub async fn haptics_impact_light();

    #[wasm_bindgen]
    /// Vibrate the device.
    pub async fn haptics_vibrate();

    #[wasm_bindgen]
    /// Trigger a selection started haptic hint.
    pub async fn haptics_selection_start();

    #[wasm_bindgen]
    /// Trigger a selection changed haptic hint. If a selection was started already, this
    /// will cause the device to provide haptic feedback.
    pub async fn haptics_selection_changed();

    #[wasm_bindgen]
    /// If selectionStart() was called, selectionEnd() ends the selection. For example,
    /// call this when a user has lifted their finger from a control.
    pub async fn haptics_selection_end();
}
```

Let’s break down what’s going on here:

- We mirror some of the types from the Capacitor plugin library into Rust to `ImpactOptions`, `ImpactStyle` (unfortunately `wasm_bindgen` doesn’t support generating these for us, but it’s more or less a copy-paste of the TypeScript code)
- We point to our generated JavaScript file `#[wasm_bindgen(module = "/js-dist/haptics.js")]` which will ensure it gets included automatically
- We setup the type signatures matching each of the TypeScript functions we defined and that we want to include

We also need to add a `src/plugins/mod.rs` file to expose our new plugin:

```rust name=capacitor-rs/src/plugins/mod.rs
pub mod haptics;
```

We’re now ready to use it in our App by adding the new `capacitor-rs` crate to our dependencies in our WASM app:

```toml name=capacitor-rs/Cargo.toml
# ...
[dependencies]
capacitor-rs = { version = "0.1.0", path = "../capacitor-rs" }
# ...other dependencies
```

And then using it like you like a normal Rust function:

```rust name=example-component.rs
use log::info;
use capacitor_rs::plugins::haptics;
use leptos::*;

#[component]
#[allow(non_snake_case)]
pub fn Page() -> impl IntoView {
    let haptic_feedback = create_action(|_: &()| {
        info!("Testing haptic_feedback");
        async move {
            let style = haptics::ImpactStyle::Light;
            haptics::haptics_impact(haptics::ImpactOptions {
                style: style.into(),
            })
        }
    });
    view! {
        <span on:click=move |_| { haptic_feedback.dispatch(()) }>
            Click to test haptic feedback
        </span>
    }
}
```

And that’s it!

Since I can’t exactly screenshot a haptic vibration, here’s an example where we use the [Capacitor](https://capacitorjs.com/docs/basics/utilities) utility function to determine which platform we are on, set up in the same way:

<div class="clear two-images">
  <a href="/resources/images/using-capacitor-plugins-from-rust-wasm-iphone.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/using-capacitor-plugins-from-rust-wasm-iphone.thumbnail.png" loading="lazy" alt="Screenshot of Capacitor running on iPhone" title="Screenshot of Capacitor running on iPhone" style="margin-right: 1%; width: 49%;" /></a>
  <a href="/resources/images/using-capacitor-plugins-from-rust-wasm-web.png" target="_blank" rel="noopener noreferrer"><img src="/resources/images/using-capacitor-plugins-from-rust-wasm-web.thumbnail.png" loading="lazy" alt="Screenshot of Capacitor running in Web" title="Screenshot of Capacitor running in Web" style="margin-left: 1%; width: 49%;" /></a>
</div>
<div class="clear"></div>
