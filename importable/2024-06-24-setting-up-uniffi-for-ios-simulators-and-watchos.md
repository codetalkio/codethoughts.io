---
title: "Setting up UniFFI for iOS, Simulators, and watchOS"
tags: rust, wasm, leptos, mobile
date: 2024-06-24
---

This is a part of the post [Mobile: A different way using Rust?](/posts/2024-06-25-mobile-a-different-way.html).

There are some great resources out there on UniFFI already such as [this post](https://forgen.tech/en/blog/post/building-an-ios-app-with-rust-using-uniffi), but it doesn’t cover watchOS, so let’s take a quick tour through what I’ve set up in the example repository [https://github.com/Tehnix/template-mobile-wasm](https://github.com/Tehnix/template-mobile-wasm)[.](https://github.com/Tehnix/playground-mobile-wasm?tab=readme-ov-file)

We've set up four crates:

- `appy`: Our Leptos App, Capacitor, and the XCode project
- `capacitor-rs`: Bridging code between the Capacitor JS library and our Rust code
- `shared`: Our shared code that we might use in `appy`, and also want to expose in Swift to use in our Widgets or watchOS App
- `mobile`: Where we will generate the Swift bindings from via UniFFI, reexporting everything from `shared` that’s made available to UniFFI via the macros

I won’t go over the details to get these to play nicely with Cargo and Workspaces, check out the repository for that. Let’s instead focus on a simplified version of what `mobile` does (the rest assumes you’re in the `mobile/` directory).

<div></div><!--more-->

If starting from scratch, create a new cargo project:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(1).sh

Update your `Cargo.toml`:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=mobile%5CCargo.toml

Update your `src/lib.rs`:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=mobile%5Csrc%5Clib.rs

We’re using `setup_scaffolding` to avoid needing to manually construct headers, modulemaps, and the UDL files (check out the [docs here](https://mozilla.github.io/uniffi-rs/0.27/tutorial/Rust_scaffolding.html#setup-for-crates-using-only-proc-macros)).

<div class="callout">
  <div class="callout-bulb">💡</div>
  If you are rexporting another crate, like I am in the example repository, you can replace all of the contents of `src/lib.rs` with this line pointing to your other crate, e.g. “shared”, `shared::uniffi_reexport_scaffolding!();` ([docs here](https://mozilla.github.io/uniffi-rs/0.27/tutorial/Rust_scaffolding.html#libraries-that-depend-on-uniffi-components)).
</div>

And finally, create a new file `src/bin/uniffi-bindgen.rs`:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=mobile%5Csrc%5Cbin%5Cuniffi-bindgen.rs

We’re now ready to build the binary for generating our bindings, and then use that to generate the actual bindings:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(5).sh

We also need to rename the FFI file to `module.modulemap` so that XCFramework will find it:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(6).sh

Now, let's add support for iOS, the Simulator and macOS via rustup:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(7).sh

and then build the library for all of our targets:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(8).sh

We'll combine `x86_64-apple-ios` and `aarch64-apple-ios-sim` into a single binary later on, but for now we keep them separate.

If we want watchOS we need to handle things a bit differently, since these are Tier 3 targets (i.e. rustup won't have their stdlib):

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(9).sh

That's a lot of targets, which represent all the various Watch models, as well as the simulators (we always need both ARM and x86).

`xcodebuild` won't be happy if we just drop them in individually, so we need to create a fat binary:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(10).sh

We can then create our XCFramework:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(11).sh

And finally, we'll combine `x86_64-apple-ios` and `aarch64-apple-ios-sim` into a single binary. If we included both of these in the XCFramework, `xcodebuild` would complain that these are the same, and not generate our XCFramework file. Oddly enough, it will not be able to build the project without both, so we let `xcodebuild` generate the XCFramework first, and then replace the binary with the fat binary:

https://gist.github.com/Tehnix/74dfea95fe6b38de0fdd10050b20adb3.js?file=terminal%20(12).sh

Done!

As the final step we drag-n-drop `./ios/Shared.xcframework` and `./bindings/shared.swift` into the XCode project whereever you want them. I personally like to create a new group (folder) called `Generated` for them (the `build-ios.sh` script assumes that's the case)..
