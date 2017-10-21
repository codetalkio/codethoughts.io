# Quick walkthrough of Hakyll
If you change something in `site.hs`, then it needs to be recompiled, using GHC. Everything else, should just need a rebuild via Hakyll.

```bash
$ stack build
```

After this, rebuilding the site is as simple as,

```bash
$ stack exec -- hakyll rebuild
```

or, alternatively use `watch` to launch a preview server while developing,

```bash
$ stack exec -- hakyll watch
```


# Compiling .scss
The hakyll setup features a compressScssCompiler, which relies on the `sass` tool being installed (e.g. `gem install compass`). It automatically compiles the SCSS from `scss/app.scss`, and puts it in `_site/app.css`.

Alternatively it is possible to just use the `sass` tool directly. For the initial compile, use,

```bash
$ sass resources/scss/app.scss:_site/app.css
```

and while developing, you can automatically compile it using,

```bash
$ sass --watch resources/scss/app.scss:_site/app.css
```


# Uploading the site
Simply run `deploy.sh` and the site will be uploaded along with the CloudFlare cache purged.


# 403 on images
This is caused by missing permissions, and can be fixed by running `chmod -R +rx /usr/share/nginx/codetalk.io/images` on the folder on the server.
