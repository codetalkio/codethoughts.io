# codetalk.io
Hakyll website for the codetalk.io blog.

# Quick walkthrough of Hakyll
If you change something in `site.hs`, then it needs to be recompiled, using GHC. Everything else, should just need a rebuild via Hakyll.

```
$ stack build
```

and then copy the binary file to the root, if you want to.

After this, rebuilding the site is as simple as,

```
$ ./hakyll rebuild
```

or, alternatively use `watch` to launch a preview server while developing,

```
$ ./hakyll watch
```

Or all in one line,

```
$ ghc --make -threaded site.hs && ./hakyll rebuild && ./hakyll watch
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


# Uploading the site
To remove some of the hassle associated with updating the site, a git pre-push
hook can be created.

Copy the following into `.git/hooks/pre-push` and `chmod +x pre-push` to make
it executable.

```bash
# Get the project root
rootDir=$(git rev-parse --show-toplevel)

# Clean and build the site
$rootDir/hakyll clean
$rootDir/hakyll build

# Upload (rsync) the site to the remote server
rsync -rae "ssh -i codetalk-io.pem" $rootDir/_site/*
ec2-user@codetalk:/usr/share/nginx/codetalk.io --delete-after
```
