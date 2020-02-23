# [codetalk.io](https://codetalk.io) site [![build status](https://github.com/codetalkio/codetalk.io/workflows/cd/badge.svg)](https://github.com/codetalkio/codetalk.io/actions)

## Dependencies

You'll need sass/compass to compile the (S)CSS files, else `hakyll` will complain in the build phase.

```bash
$ gem install compass # run `rbenv install 2.6.5` if you needed
```

## Quick walkthrough of Hakyll

If you change something in `site.hs`, then it needs to be recompiled, using GHC. Everything else, should just need a rebuild via Hakyll.

```bash
$ stack build
```

After this, rebuilding the site is as simple as,

```bash
$ stack exec -- hakyll-site rebuild
```

or, alternatively use `watch` to launch a preview server while developing,

```bash
$ stack exec -- hakyll-site watch
$ sass resources/scss/app.scss:_site/app.css --style compressed --watch
```

## Image Optimization

```bash
$ brew install imagemagick
```

Either put the images in `resources/images/unoptimized`, or copy them with the following script,

```bash
$ cd resources/images
$ mkdir -p unoptimized
$ for f in *.png; do cp "$f" "unoptimized/${f%.png}.thumbnail.png"; done
```

Now that the images you want to generate optimized versions for are in `resources/images/unoptimized`, run the optimization script inside this folder,

```bash
$ cd unoptimized
$ mogrify -resize 50% *.thumbnail.png
```

Finally, copy them out to images again.
