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

I won’t go over the details to get these to play nicely with Cargo and Workspaces, check out the repository for that. Let’s instead focus on what `mobile` does (the rest assumes you’re in the `mobile/` directory).

<div></div><!--more-->

If starting from scratch, create a new cargo project:

```bash
cargo init mobile --lib
```

Update your `Cargo.toml`:

```toml
[package]
name = "mobile"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["staticlib"]
name = "mobile"

[[bin]]
name = "uniffi-bindgen"
path = "src/bin/uniffi-bindgen.rs"

[profile.release]
opt-level = 'z' # Optimize for size.
lto = true      # Enable Link Time Optimization.
strip = true    # Automatically strip symbols from the binary.
panic = "abort"
debug = false
# Optional:
# codegen-units = 1 # Reduce Parallel Code Generation Units to Increase Optimization.

[dependencies]
# UniFFI dependencies for generating Swift bindings.
uniffi = { version = "0.28.0", features = ["cli"] }
```

Update your `src/lib.rs`:

```rust
uniffi::setup_scaffolding!();

#[derive(uniffi::Enum)]
pub enum Fruits {
    Watermelon,
    Cranberry,
    Cherry,
}

#[uniffi::export]
pub fn eat_fruit(fruit: Fruits) -> String {
    match fruit {
        Fruits::Watermelon => "Eating Watermelon".to_string(),
        Fruits::Cranberry => "Eating Cranberry".to_string(),
        Fruits::Cherry => "Eating Cherry".to_string(),
    }
}
```

We’re using `setup_scaffolding` to avoid needing to manually construct headers, modulemaps, and the UDL files (check out the [docs here](https://mozilla.github.io/uniffi-rs/0.27/tutorial/Rust_scaffolding.html#setup-for-crates-using-only-proc-macros)).

<div class="callout">
  <div class="callout-bulb">💡</div>
  If you are rexporting another crate, like I am in the example repository, you can replace all of the contents of `src/lib.rs` with this line pointing to your other crate, e.g. “shared”, `shared::uniffi_reexport_scaffolding!();` ([docs here](https://mozilla.github.io/uniffi-rs/0.27/tutorial/Rust_scaffolding.html#libraries-that-depend-on-uniffi-components)).
</div>

And finally, create a new file `src/bin/uniffi-bindgen.rs`:

```rust
fn main() {
    uniffi::uniffi_bindgen_main()
}
```

We’re now ready to build the binary for generating our bindings, and then use that to generate the actual bindings:

```bash
# Build the uniffi-bindgen binary and our initial library file.
cargo build
cargo run --bin uniffi-bindgen generate --library ./target/debug/libmobile.a --language swift --out-dir ./bindings

```

We also need to rename the FFI file to `module.modulemap` so that XCFramework will find it:

```bash
mv ./bindings/sharedFFI.modulemap ./bindings/module.modulemap

```

Now, let's add support for iOS, the Simulator and macOS via rustup:

```bash
rustup target add aarch64-apple-darwin
rustup target add aarch64-apple-ios
rustup target add aarch64-apple-ios-sim
rustup target add x86_64-apple-ios # iOS simulator, also needed on Arm Macs.

```

and then build the library for all of our targets:

```bash
carbo build --release --target=aarch64-apple-darwin
carbo build --release --target=aarch64-apple-ios
carbo build --release --target=aarch64-apple-ios-sim
carbo build --release --target=x86_64-apple-ios

```

We'll combine `x86_64-apple-ios` and `aarch64-apple-ios-sim` into a single binary later on, but for now we keep them separate.

If we want watchOS we need to handle things a bit differently, since these are Tier 3 targets (i.e. rustup won't have their stdlib):

```bash
cargo +nightly build -Zbuild-std=std,panic_abort --release --target=aarch64-apple-watchos-sim
cargo +nightly build -Zbuild-std=std,panic_abort --release --target=x86_64-apple-watchos-sim
cargo +nightly build -Zbuild-std=std,panic_abort --release --target=aarch64-apple-watchos
cargo +nightly build -Zbuild-std=std,panic_abort --release --target=armv7k-apple-watchos
cargo +nightly build -Zbuild-std=std,panic_abort --release --target=arm64_32-apple-watchos

```

That's a lot of targets, which represent all the various Watch models, as well as the simulators (we always need both ARM and x86).

`xcodebuild` won't be happy if we just drop them in individually, so we need to create a fat binary:

```bash
# Combine the watchOS simulator libraries into a single file using lipo.
mkdir -p target/watchOS-sim/release
lipo -create target/aarch64-apple-watchos-sim/release/libmobile.a \\
target/x86_64-apple-watchos-sim/release/libmobile.a \\
        -o target/watchOS-sim/release/libmobile.a
# Confirm the architectures.
lipo -info target/watchOS-sim/release/libmobile.a

# Combine the watchOS libraries into a single file using lipo.
mkdir -p target/watchOS/release
lipo -create target/aarch64-apple-watchos/release/libmobile.a \\
        target/arm64_32-apple-watchos/release/libmobile.a \\
        target/armv7k-apple-watchos/release/libmobile.a \\
        -o target/watchOS/release/libmobile.a
# Confirm the architectures.
lipo -info target/watchOS/release/libmobile.a

```

We can then create our XCFramework:

```bash
xcodebuild -create-xcframework \\
    -library ./target/aarch64-apple-ios-sim/release/libmobile.a -headers ./bindings \\
    -library ./target/aarch64-apple-ios/release/libmobile.a -headers ./bindings \\
    -library ./target/aarch64-apple-darwin/release/libmobile.a -headers ./bindings \\
    -library ./target/watchOS-sim/release/libmobile.a -headers ./bindings \\
    -library ./target/watchOS/release/libmobile.a -headers ./bindings \\
    -output "ios/Shared.xcframework"

```

And finally, we'll combine `x86_64-apple-ios` and `aarch64-apple-ios-sim` into a single binary. If we included both of these in the XCFramework, `xcodebuild` would complain that these are the same, and not generate our XCFramework file. Oddly enough, it will not be able to build the project without both, so we let `xcodebuild` generate the XCFramework first, and then replace the binary with the fat binary:

```bash
# We need to combine the architectures for the iOS Simulator libraries after we've
# constructed the XCFramework, otherwise it will complain about them being the same,
# while also failing because of missing x86_64 if we omit it.
mkdir -p target/iOS-sim/release
lipo -create target/aarch64-apple-ios-sim/release/libmobile.a \\
  target/x86_64-apple-ios/release/libmobile.a \\
  -o target/iOS-sim/release/libmobile.a
# Confirm the architectures.
lipo -info target/iOS-sim/release/libmobile.a
# Move it into place.
rm ios/Shared.xcframework/ios-arm64-simulator/libmobile.a
cp target/iOS-sim/release/libmobile.a ios/Shared.xcframework/ios-arm64-simulator/libmobile.a

```

Done!

As the final step we drag-n-drop ./ios/Shared.xcframework and ./bindings/shared.swift into the XCode project whereever you want them. I personally like to create a new group (folder) called `Generated` for them (the `build-ios.sh` script assumes that's the case).
