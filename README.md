# Codetalk.io site

[![Build Status](https://travis-ci.org/codetalkio/codetalk.io.svg?branch=master)](https://travis-ci.org/codetalkio/codetalk.io)

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
```
