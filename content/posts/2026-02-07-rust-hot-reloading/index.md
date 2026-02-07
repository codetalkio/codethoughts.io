+++
title = "Hot Reloading in Rust? Subsecond and Dioxus to the rescue!"
date = 2026-02-07

[taxonomies]
tags = ["rust", "hot-reloading", "dx", "devtools", "dioxus", "subsecond"]

+++

I recently set out to finally get hot reloading working for my Rust GraphQL API, after getting annoyed by the usual `cargo watch` flow, which would kill my server in between code changes and it finishing the rebuild + setup.

I had previously played around with [subsecond](https://crates.io/crates/subsecond) from [Dioxus](https://dioxuslabs.com/), while it was still in beta, but hadn't had much success. Sometime in August last year (2025), a few improvements got added in [PR #4588](https://github.com/DioxusLabs/dioxus/pull/4588), namely convenience functions for serving async functions with hot reloading. I did wish their own docs on [hot reloading](https://dioxuslabs.com/learn/0.7/essentials/ui/hotreload/) would have mentioned it 😮‍💨

So it was time to give it another try!

<div></div><!-- more -->

The process was pretty straightforward:

1.  Add dependency: `cargo add dioxus_devtools --features serve`
2. Wrap your entry point with `dioxus_devtools::serve_subsecond_with_args`
3. Install dioxus-cli: `curl -sSL https://dioxus.dev/install.sh | bash` (see [docs](https://dioxuslabs.com/learn/0.7/getting_started/))
4. Run: `dx serve --hot-patch`
5. 🎉


I'll add a few more details to each of the steps below.

{{ toc() }}

## Dependencies

The only dependency we need is the [dioxus-devtools](https://crates.io/crates/dioxus-devtools) crate (version `0.7.3` as of writing this).

Importantly we need to add the `serve` feature to it (`cargo add dioxus_devtools --features serve`). This is what gives us `dioxus_devtools::serve_subsecond_with_args`.


## Passing our function to subsecond

The restructure of our application's entry point was one of the less obvious things, at least until [#4588](https://github.com/DioxusLabs/dioxus/pull/4588) got merged. We'll do it in three simple steps:


1. Split out any setup of env, database, tracing, etc, so its done first
2. Split out your main async server function
3. Pass the setup and server function to dioxus_devtools::serve_subsecond_with_args

This could look something like this, with your main entry point being:

```rust ,linenos,hl_lines=4 5 6 7
#[tokio::main]
async fn main() {
    let app_env = setup_app_env().await.unwrap();
    dioxus_devtools::serve_subsecond_with_args(
        app_env,
        |s| async { server(s).await }
    ).await;
}
```


{% aside() %}
  I'd recommend putting it behind a feature flag such as `local` or similar. See [further down](#putting-behind-a-feature-flag) for more details.
{% end %}


And your `setup_app_env` looking like this:

```rust ,linenos
#[derive(Clone)]
struct ApplicationEnv {
    port: i32,
    main_db: Arc<Db>,
}

async fn setup_app_env() -> Result<ApplicationEnv, Error> {
  let port = env::var("MS_GRAPHQL_PORT")
        .map(|p| p.parse::<i32>().expect("MS_GRAPHQL_PORT must be a number"))
        .unwrap_or(3065);
  let main_db = Arc::new(Db::new(main_url, main_token).await?);
  Ok(ApplicationEnv { port, main_db })
}
```

and `server` could look something like:

```rust ,linenos
async fn server(config: ApplicationEnv) -> Result<(), Error> {
  use axum::{Router, routing::get};

  let ApplicationEnv { port, main_db } = config;
  let app = Router::new().route("/", get(test_route));
  let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", port)).await.unwrap();
  println!("Server running on http://localhost:{}", port);
  axum::serve(listener, app.clone()).await.unwrap()
}
```

You'll of course need to adjust this to your actual code, but I thought I'd give a slightly more complex example to illustrate the point.

## Running the hot reloading server

Finally, we'll be running the hot reloading server with using the [dioxus cli](https://dioxuslabs.com/learn/0.7/getting_started/) which takes care of coordinating with subsecond for rebuilding our app. Follow their instructions to install it (or run `curl -sSL https://dioxus.dev/install.sh | bash`), and then you should now have access to `dx serve --hot-patch` which will start the server with hot reloading, looking something like this:

```bash
╭──────────────────────────────────────────────────────────────────────────────── /:more ╮
│  App:     ━━━━━━━━━━━━━━━━━━━━━━━━━━  🎉           Platform: MacOS                     │
│  Bundle:  ━━━━━━━━━━━━━━━━━━━━━━━━━━  🎉           Features: ["local"]                 │
│  Status:  Serving ms-graphql 🚀                    Server at: no server address        │
╰────────────────────────────────────────────────────────────────────────────────────────╯
```

Beautiful! 🤌

You can now code away happily and speed up your fullstack work!

I've been using this for a few days now for our main GraphQL API serving the backend for [Yaay](https:://yaaytravel.com), and it's been a quite nice addition as a DX improvement.

A big thanks to the amazing team at Dioxus for making subsecond possible to use outside of the Dioxus framework, and for pushing the frontier of Web dev-like DX in Rust!


## Putting behind a feature flag

As mentioned earlier, you probably don't want this out in production, so I'd recommend putting it behind a feature flag. I've personally gone with `local`.

You'll need a small change to your entry point:

```rust ,linenos,hl_lines=1 8
#[cfg(not(feature = "local"))]
#[tokio::main]
async fn main() {
    let app_env = setup_app_env().await.unwrap();
    server(app_env).await;
}

#[cfg(feature = "local")]
#[tokio::main]
async fn main() {
    let app_env = setup_app_env().await.unwrap();
    dioxus_devtools::serve_subsecond_with_args(
        app_env,
        |s| async {
            server(s).await
        }
    ).await;
}
```

and you can also adjust your dependencies to only include the `dioxus-devtools` crate when the `local` feature is enabled:

```toml ,linenos,hl_lines=2 3 4 5 6 9
# ...rest of your Cargo.toml
[features]
# Add optional dependencies when the `local` feature is enabled
local = [
  "dep:dioxus-devtools",
]

[dependencies]
dioxus-devtools = { version = "0.7.3", optional = true, features = ["serve"] }
# ..rest of your dependencies
```

## Resources

Some links that were helpful in piecing all of this together:

- [dioxus#4588](https://github.com/DioxusLabs/dioxus/pull/4588)
- [dioxus/notes/architecture/07-HOTRELOAD.md](https://github.com/DioxusLabs/dioxus/blob/9b0d380765301ae7fe337fba8438fcbb814f8748/notes/architecture/07-HOTRELOAD.md)
- [dioxus/packages/devtools/src/lib.rs#L113-L220](https://github.com/DioxusLabs/dioxus/blob/9b0d380765301ae7fe337fba8438fcbb814f8748/packages/devtools/src/lib.rs#L113-L220)
- [Phosphorus-M/spring-rs/blob/hot-reloading/examples/hot-reloading/src/main.rs](https://github.com/Phosphorus-M/spring-rs/blob/hot-reloading/examples/hot-reloading/src/main.rs)


{{ medium_comments(post="hot-reloading-in-rust-subsecond-and-dioxus-to-the-rescue-fdc3d74f7ec9") }}
