set dotenv-load

# Display help information.
help:
  @ just --list

# Open project workspace in VS Code.
code:
  @ code codetalk.code-workspace

# Install tooling for working with the codetalk blog.
[linux]
install-tooling:
  @ just _install-tooling-all-platforms
  # Install imagemagick for mogrify.
  command -v mogrify >/dev/null 2>&1 || sudo apt install imagemagick
  gh release download --clobber --pattern "hakyll-site" --dir ./dist
  chmod +x ./dist/hakyll-site

# Install tooling for working with the codetalk blog.
[macos]
install-tooling:
  @ just _install-tooling-all-platforms
  # Install imagemagick for mogrify.
  command -v mogrify >/dev/null 2>&1 || brew install imagemagick

_install-tooling-all-platforms:
  # Install stack.
  command -v stack >/dev/null 2>&1 || curl -sSL https://get.haskellstack.org/ | sh
  # Install ghcup.
  command -v ghcup >/dev/null 2>&1 || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  # Install sass.
  command -v sass >/dev/null 2>&1 || npm i -g sass

# Setup dependencies and build the hakyll executable.
setup:
  stack build

# Deploy the blog to S3 and invalidate CloudFront cache.
deploy:
  #!/usr/bin/env bash
  set -euxo pipefail
  just build
  # Sync files to S3.
  aws s3 sync _site s3://codetalk.io --delete
  # Invalidate Cloudflare cache.
  curl -X DELETE "https://api.cloudflare.com/client/v4/zones/${CLOUDFLARE_ZONE}/purge_cache" \
      -H "Authorization: Bearer ${CLOUDFLARE_TOKEN}" \
      -H "Content-Type: application/json" \
      --data '{"purge_everything":true}'

# Run hakyll development server in watch mode.
dev:
  stack exec -- hakyll-site watch --port 4566

# Build blog artifacts and static files.
build:
  ./dist/hakyll-site clean || stack exec -- hakyll-site clean
  ./dist/hakyll-site clean || stack exec -- hakyll-site build
  sass resources/scss/app.scss:_site/app.css --style compressed
