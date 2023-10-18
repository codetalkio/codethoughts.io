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
  sudo apt install imagemagick

# Install tooling for working with the codetalk blog.
[macos]
install-tooling:
  @ just _install-tooling-all-platforms
  # Install imagemagick for mogrify.
  brew install imagemagick

_install-tooling-all-platforms:
  # Install stack.
  curl -sSL https://get.haskellstack.org/ | sh
  # Install ghcup.
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  # Install sass.
  npm i -g sass

# Setup dependencies and build the hakyll executable.
setup project:
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
dev project:
  stack exec -- hakyll-site watch

# Build blog artifacts and static files.
build:
  ./dist/hakyll-site clean || stack exec -- hakyll-site clean
  ./dist/hakyll-site clean || stack exec -- hakyll-site build
  sass resources/scss/app.scss:_site/app.css --style compressed
