set dotenv-load

# Display help information.
help:
  @ just --list

# Open project workspace in VS Code.
code:
  @ code codetalk.code-workspace

# Install tooling for working with the codethoughts blog.
[linux]
install-tooling:
  @ just _install-tooling-all-platforms
  echo "Currently unsupported, see https://www.getzola.org/documentation/getting-started/installation/ for installation instructions."

# Install tooling for working with the codethoughts blog.
[macos]
install-tooling:
  @ just _install-tooling-all-platforms
  # Install zola.
  brew install zola

_install-tooling-all-platforms:
  echo "Setting up tools..."

# Run zola development server in watch mode.
dev:
  bunx concurrently --names tailwind,zola "bunx tailwindcss --input styles/input.css --output static/styles/output.css --watch" "zola serve --port 4566"

# Build blog artifacts and static files.
build:
  bunx tailwindcss --input styles/input.css --output static/styles/output.css --minify
  zola build

# Generate posts with Gists instead of code blocks.
generate-gists:
  cd tools && bun run index.ts

# Bundle and minify standalone JavaScript files.
build-js:
  cd static/js && bun build --minify --sourcemap=external --outdir toc toc-sidebar.js
  mv static/js/toc/toc-sidebar.js static/js/toc-sidebar.min.js
  mv static/js/toc/toc-sidebar.js.map static/js/toc-sidebar.min.js.map

# TODO: Move deployment to CloudFlare Pages?
# Deploy the blog to S3 and invalidate CloudFront cache.
# deploy:
#   #!/usr/bin/env bash
#   set -euxo pipefail
#   just build
#   # Sync files to S3.
#   aws s3 sync _site s3://codetalk.io --delete
#   # Invalidate Cloudflare cache.
#   curl -X DELETE "https://api.cloudflare.com/client/v4/zones/${CLOUDFLARE_ZONE}/purge_cache" \
#       -H "Authorization: Bearer ${CLOUDFLARE_TOKEN}" \
#       -H "Content-Type: application/json" \
#       --data '{"purge_everything":true}'
