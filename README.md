# codetalk.io
Hakyll website for the codetalk.io blog.

# Quick walkthrough of Hakyll
If you change something in `site.hs`, then it needs to be recompiled, using GHC. Everything else, should just need a rebuild via Hakyll.

```
$ ghc --make -threaded site.hs
```

After this, rebuilding the site is as simple as,

```
$ ./site rebuild
```

or, alternatively use `watch` to launch a preview server while developing,

```
$ ./site watch
```

Or all in one line,

```
$ ghc --make -threaded site.hs && ./site rebuild && ./site watch
```

# Compiling .scss
For the initial compile, use,

```
$ sass css/main.scss:css/main.css
```

and while developing, you can automatically compile it using,

```
$ sass --watch css/main.scss:css/main.css
```
