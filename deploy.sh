# Get the current dir
startDir=$(pwd)
# Get the project root
rootDir=$(git rev-parse --show-toplevel)

cd $rootDir

# Clean and build the site
stack exec hakyll clean
stack exec hakyll build
# Build the CSS
sass resources/scss/app.scss:_site/app.css --style compressed

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
