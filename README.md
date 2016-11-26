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
The hakyll setup features a compressScssCompiler, which relies on the `sass` tool being installed (e.g. `gem install sass`). It automatically compiles the SCSS from `scss/app.scss`, and puts it in `_site/app.css`.

Alternatively it is possible to just use the `sass` tool directly. For the initial compile, use,

```
$ sass resources/scss/app.scss:_site/app.css
```

and while developing, you can automatically compile it using,

```
$ sass --watch resources/scss/app.scss:_site/app.css
```


# Uploading the site
To remove some of the hassle associated with updating the site, a git pre-push
hook can be created.

Copy the following into `.git/hooks/pre-push` and `chmod +x .git/hooks/pre-push`
to make it executable.

```bash
# Get the current dir
startDir=$(pwd)
# Get the project root
rootDir=$(git rev-parse --show-toplevel)

cd $rootDir

# Clean and build the site
stack exec hakyll clean
stack exec hakyll build
# Build the CSS
sass scss/app.scss:_site/app.css --style compressed

# Upload (rsync) the site to the remote server
rsync -rave ssh $rootDir/_site/* ec2-user@codetalk.io:/usr/share/nginx/codetalk.io --delete-after

# Set the right permissions on the images folder
ssh ec2-user@codetalk.io "chmod -R +rx /usr/share/nginx/codetalk.io/resources/images"

# Go back to original dir
cd $startDir

# Purge the CloudFlare cache
curl -X DELETE "https://api.cloudflare.com/client/v4/zones/${CF_ZONE}/purge_cache" \
    -H "X-Auth-Email: ${CF_EMAIL}" \
    -H "X-Auth-Key: ${CF_API_TOKEN}" \
    -H "Content-Type: application/json" \
    --data '{"purge_everything":true}'

echo "Done running pre-hook!"
```

# 403 on images
This is caused by missing permissions, and can be fixed by running `chmod -R +rx /usr/share/nginx/codetalk.io/images` on the folder on the server.
