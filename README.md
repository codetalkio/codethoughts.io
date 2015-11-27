# codetalk.io
Hakyll website for the codetalk.io blog.


# Quick walkthrough of Hakyll
If you change something in `site.hs`, then it needs to be recompiled, using GHC. Everything else, should just need a rebuild via Hakyll.

```
$ stack build
```

After this, rebuilding the site is as simple as,

```
$ stack exec hakyll rebuild
```

or, alternatively use `watch` to launch a preview server while developing,

```
$ stack exec hakyll watch
```


# Compiling .scss
For the initial compile, use,

```
$ sass scss/app.scss:_site/app.css
```

and while developing, you can automatically compile it using,

```
$ sass --watch scss/app.scss:_site/app.css
```

NOTE: that these are actually be compiled on the build, by hakyll itself.


# Uploading the site
To remove some of the hassle associated with updating the site, a git pre-push
hook can be created.

Copy the following into `.git/hooks/pre-push` and `chmod +x pre-push` to make
it executable.

```bash
# Get the current dir
startDir=$(pwd)
# Get the project root
rootDir=$(git rev-parse --show-toplevel)

cd $rootDir

# Clean and build the site
stack exec hakyll clean
stack exec hakyll build

# Upload (rsync) the site to the remote server
rsync -rave ssh $rootDir/_site/* ec2-user@codetalk:/usr/share/nginx/codetalk.io
--delete-after

# Set the right permissions on the images folder
ssh ec2-user@codetalk "chmod -R +rx /usr/share/nginx/codetalk.io/images"

# Go back to original dir
cd $startDir
```

# 403 on images
This is caused by missing permissions, and can be fixed by running `chmod -R +rx /usr/share/nginx/codetalk.io/images` on the folder on the server.
