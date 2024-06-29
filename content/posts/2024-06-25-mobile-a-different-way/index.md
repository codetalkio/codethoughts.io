+++
title = "Mobile: A different way using Rust?"
date = 2024-06-25
[taxonomies]
tags = ["rust", "wasm", "leptos", "mobile", "capacitor", "swift", "xcode"]
+++

After having spent the better part of two months deep in the trenches of iOS development with Swift, I can comfortably say: Web Development simply is more mature.

Let’s just run over a few things I ran into that’s bad, broken, or made for a poor experience:

- A lot of issues with data handling in general
    - Turning iCloud on when using SwiftData will restrict core functionality such as the ability to have unique IDs on your data, meaning everyone now has to handle duplicates
    - iCloud (CloudKit) and data migrations don't work really work, it just won’t run it making data changes in your models quite the chore
    - SwiftData generally has very bad performance, both in reads, writes, and inserts
    - It’s quite hard to control memorization and rendering performance (e.g. `@Query` in your `View`)
- Swift itself is a bit of an *eh* language - it has many good parts and ergonomics, but also some weird design choices that make scaling codebases messy such as everything by default living in the global namespace and file names can’t clash
- SwiftUI is generally great, except when you want to do anything non-standard, in which case you’ll quickly feel the pain
- The feedback loop is slow: If you can run things in the Preview that’s the fastest, otherwise it’s a full Compile→Run→Simulator loop
- UI performance issues are very easy to sneak in, and very hard to track down
- Apple’s frameworks sure do have a lot of bugs and you’ll commonly see stuff being broken almost permanently

Admittedly, these might be trivial gripes for someone that has spent years in the Mobile ecosystem, but they certainly didn’t make me fall in love with it. More importantly, it wasn’t really providing a great user experience either.

….so what’s the alternative?

<div></div><!-- more -->

{% aside() %}
  Looking for the TL;DR? Checkout [https://github.com/Tehnix/template-mobile-wasm](https://github.com/Tehnix/template-mobile-wasm) for a ready setup with Rust/Leptos, i18n, TailwindCSS, Capacitor + bindings, iOS/Android, Widget, watchOS, and code sharing to Swift + Kotlin.
{% end %}

Here's an overview of what we'll cover:

{{ toc() }}

## The goals

Before being able to explore the solution space we need to make clear what we’re trying to achieve here, as this will quickly start excluding options.

We want to:

- **One language**: Use one language across the entire stack (Frontend, Mobile, Backend). In other words, we should be able to write *one* App in our “core” language and deploy across Web and Mobile App (i.e. something that goes in the App Store)
- **All Targets**: Support for all mobile targets: iOS + Widgets, watchOS + Complications, macOS + Widgets
- **Code Sharing**: If we must drop into native code (Swift/Kotlin) we should be able to use code from our “core” language

## **One language**

> To rule them all?..

This admittedly does not start to limit our choices too much yet, so what do we base our choice on here?

For me personally, I want something performant and ergonomic. To me that essentially boils it down to Rust, which will scale amazingly on the Backend (especially in Serverless), and has very good ergonomics for working in it. It features both a very helpful compiler, a vibrant community, and macros to paper over any boilerplate code.

Rust supports Frontend development via WASM, supported in all major browsers, and there are very good frameworks such as Leptos, Dioxus, and many more.

Good alternatives for a full-stack language would be TypeScript and Node.js/Bun. For reasons we’ll see later, this is not the choice we go with in the end, but for now let’s consider it a viable alternative (it might be for you!).

Now, how do we get our Rust or TypeScript code onto our different deployment targets? Let’s make an overview of how you’d typically do. For good measure, let’s also compare with Dart/Flutter:


| Language / Target | Backend Options  | Frontend Options         | Mobile Options                |
|-------------|------------------|--------------------------|-------------------------------|
| Rust        | - Native Support | - Leptos (WASM) <br>- Dioxus (WASM) <br>- Yew (WASM) <br>- …etc          | - Capacitor (WASM in WebView) <br>- Tauri (Beta, WASM in WebView) |
| TypeScript  | - Node.js <br>- Bun       | - Native Support (React, Vue, Svelte, Solid, etc) | - Capacitor (JS in WebView)   |
| Dart/Flutter| - Native Support | - Native for Dart <br>- Flutter (HTML or Canvas)       | - Flutter                     |

For Rust there are some interesting projects brewing such as [Robius](https://robius.rs) which is quite ambitious and hopefully gains traction.

The choice is not yet super clear here, and all could work so far. One concern I have with Dart/Flutter specifically is the lack of maturity in its Backend ecosystem currently. It you’ll quickly run into missing or unmaintained libraries.

**Choice**: Rust & WASM

**Alternative**: TypeScript + Node.js/Bun or Dart/Flutter

## **All Targets**

> It’s all or nothing!

This is were things get tricky. We’re greedy, and we want to be able to have all our options open and not limit ourselves, permanently closing off doors.

Let’s take a concrete example:

If you want to develop an iOS Widget ([those little things](https://support.apple.com/en-us/118610) on your Home/Lock screen), you need to create a new Widget Extension Target in XCode and Embed that Target into your main App. It cannot function on its own, since it inherently is an *extension* to your App.

Here’s an example of a Widget for a Todo App, providing interactivity from the Home Screen:

{{ image(path="mobile-a-different-way-widget-screenshot.jpg", caption="Screenshot of Capacitor an iOS Widget", width=450) }}

This seemingly innocuous example is also where most of our options get limited and we’ll have to get a bit creative with how we solve it.

So how does support look like for our Mobile options? (we’ll simplify for a moment to the iOS ecosystem)

| Mobile Options / Target Support | iOS App | macOS App | Widget Extension | watchOS | watchOS Complication |
| --- | --- | --- | --- | --- | --- |
| Tauri | ✅ (sorta)[^1] | ✅ (same App as the iOS App)[^2] | ❌ | ❌ | ❌ |
| Capacitor | ✅ (sorta)[^3] | ✅ (same App as the iOS App)[^2] | ✅⚠️ (via Native code) | ✅⚠️ (via Native code) | ✅⚠️ (via Native code) |
| Flutter          | ✅            | ✅                  | ✅⚠️ (via Native code) | ✅⚠️ (via Native code) | ✅⚠️ (via Native code)    |

[^1]: Platform support overview here [https://v2.tauri.app/plugin/](https://v2.tauri.app/plugin/)

[^2]: This is the “Designed for iPad” type of App which is essentially an iOS App that runs completely native on ARM-based Macbooks. Alternatively you can also deploy it as an Electron App using [https://github.com/capacitor-community/electron](https://github.com/capacitor-community/electron) for other targets.

[^3]: Platform support overview here [https://capacitorjs.com/docs/apis](https://capacitorjs.com/docs/apis)

Before continuing, let’s talk about that ✅⚠️ score though—what does that mean exactly?

At this point, no matter what solution we end up with, we will need to drop into XCode to setup our Widget Extensions, watchOS Apps, and watchOS Complications.

But will our chosen framework support that, or will we get completely blocked even trying to do this (no matter how well supported)?

For Tauri, the answer is: We are blocked ([#9766](https://github.com/tauri-apps/tauri/issues/9766) is still open in the Tauri repo), and cannot proceed.

Luckily, for both Capacitor and Dart/Flutter, that’s not entirely the case. We can open XCode and add our own Targets that will build alongside our App, for anything that we want, such as Widgets and our watchOS App.

That looks like this in XCode:

{{ image(path="mobile-a-different-way-xcode-adding-targets.png", caption="Adding additional Targets in Xcode", width=650) }}

The limitation here is that these additional targets only support native code (i.e. Swift or Kotlin/Java).

**Before jumping into the next section where we’ll look at code sharing**, how do we actually communicate with and/or use the native capabilities that Capacitor supports? I’ve written up a guide on how exactly to do that in detail here: [Using Capacitor Plugins from Rust/WASM](/posts/2024-06-24-using-capacitor-plugins-from-rust-wasm.html).

**Choice**: Capacitor

**Alternative**: Dart/Flutter

## **Code Sharing**

> If we have to, can we at least make it nice?

So, if we *must* write Native platform code for certain things, can we at least reuse some of our code across from our “core” language into our native language?

This is where we get into the differences in our language choice. Our options are:

- Calling Rust using [UniFFI](https://mozilla.github.io/uniffi-rs/latest/) and generated Swift bindings
- Calling JavaScript by evaluating it using [JavaScriptCore](https://developer.apple.com/documentation/javascriptcore)
- Calling Flutter via FFI generated using [add-to-app](https://docs.flutter.dev/add-to-app)

We can quickly exclude the [JavaScriptCore](https://developer.apple.com/documentation/javascriptcore) option as a viable route, it’s simply not gonna be even remotely ergonomic.

You essentially import your JavaScript file, or put the code in a String. Then you evaluate it, and can get the output. There’s not much interop between the host language (Swift) and the guest language (JavaScript), providing for poor ergonomics in our usecase (some examples [here](https://stackoverflow.com/a/37435316) and [here](https://douglashill.co/javascript-in-swift/)).

Dart/Flutter does support calling [Objective-C/Swift from within Dart](https://dart.dev/interop/objective-c-interop#callbacks-and-multithreading-limitations), and with a bit of work it does seem to be able to generate FFI bindings via Flutter to go the other way as well, with a few examples given in [their documentation](https://docs.flutter.dev/add-to-app).

How does the Rust side of things then look like? Very much like Dart/Flutter actually!

1. Use the [uniffi crate](https://crates.io/crates/uniffi)
2. Annotate the functions, enums, records, etc that you want to export with a macro
3. Generate your Swift bindings and headers, and build a static library file for all your relevant targets
4. Construct an `.xcframework` file from the builds
5. Import the generated Swift and `.xcframework` into XCode
6. Done!

I’ve written up a guide on how exactly to do that in detail here: [Setting up UniFFI for iOS, Simulators, and watchOS](/posts/2024-06-24-setting-up-uniffi-for-ios-simulators-and-watchos.html).

Once you’ve done the initial project setup (for building the targets and generating the `.xcframework` file) you don’t really touch that again, and you only need to concern yourself with which things to expose to Swift.

Let’s make a small example of code using `uniffi` would look like:

<!-- name=uniffi-example.rs -->
```rust ,linenos
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

How do we call the generated code? As you would any other Swift code!

<!-- name=Example.swift -->
```swift ,linenos
// Calling our Rust function with our Rust enum as an argument.
eatFruit(fruit: Fruits.watermelon)
```

<!-- name=Example.swift -->
```swift ,linenos
// It'll work everywhere you'd expect it to, e.g. in String interpolation here.
Text("Ready? \(eatFruit(fruit: Fruits.watermelon))")
```

Final result in the iOS and Android Simulators, running our Web App as a Mobile App:

{{ images(paths=["mobile-a-different-way-ios-simulator.png", "mobile-a-different-way-android-simulator.png"], captions=["Final result in the iOS Simulator", "Final result in the Android Simulator"], widths=[400, 400]) }}


Same application on the Web:

{{ image(path="mobile-a-different-way-web.png", caption="Final result on the Web", width=500) }}

The Widget Extension in the XCode Preview:

{{ image(path="mobile-a-different-way-widget-preview.png", caption="Final result as a Widget Preview in Xcode", width=600) }}

The watchOS App in the XCode Preview:

{{ image(path="mobile-a-different-way-watchos-preview.png", caption="Final result as a watchOS Preview in Xcode", width=600) }}

**Choice**: Rust with [UniFFI](https://mozilla.github.io/uniffi-rs/latest/)

**Alternative**: Dart/Flutter

## Conclusion

Let’s revisit our original goals with our final results:

- **One language**: Use one language across the entire stack (Frontend, Mobile, Backend). In other words, we should be able to write *one* App in our “core” language and deploy across Web and Mobile App (i.e. something that goes in the App Store)
    - Rust for backend
    - Rust compiled to WASM for Frontend
    - Rust compiled to WASM and packaged using Capacitor for Mobile
- **All Targets**: Support for all mobile targets: iOS + Widgets, watchOS + Complications, macOS + Widgets
    - Rust WASM + Capacitor for iOS and macOS
    - Add additional native Targets in XCode for Widgets, watchOS, and watchOS Complications
- **Code Sharing**: If we must drop into native code (Swift/Kotlin) we should be able to use code from our “core” language
    - Expose our Rust code to our native Swift code using UniFFI

Why not Dart/Flutter? My main holdback stems from the maturity the language and ecosystem has on the Backend, which seems to be quite lacking. Rust definitely has it beat here, with an extremely vibrant ecosystem.

I’m personally pretty happy with the solution. Most people probably won’t need the watchOS and Widgets, so they won’t have to touch Swift code, but it’s at least nice to know that you haven’t closed off that option for yourself down the road, as some options would leave you.

{{ medium_comments(post="mobile-a-different-way-using-rust-0e7b1dfa8cbf") }}
