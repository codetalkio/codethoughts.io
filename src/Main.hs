{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.Monoid ((<>))
import Hakyll

import Site.Compiler (sassCompiler)
import Site.Context (baseCtx, postCtx, rawPostCtx, rawPostWithTagsCtx)


-- | Define the rules for the site/hakyll compiler
main :: IO ()
main = hakyll $ do
  -- | Route for all images
  match "resources/images/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- | Route for the favicon
  match "resources/favicon.ico" $ do
    route   $ constRoute "favicon.ico"
    compile copyFileCompiler

  -- | Compile SCSS to CSS and serve it
  match "resources/scss/**.scss" $ compile getResourceBody
  scssDependencies <- makePatternDependency "resources/scss/**.scss"
  rulesExtraDependencies [scssDependencies] $
    create ["app.css"] $ do
      route idRoute
      compile sassCompiler

  -- | Load all partial templates
  match "templates/*" $ compile templateCompiler

  -- | Pages: Load all standard pages using the page template
  match "pages/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" baseCtx
      >>= loadAndApplyTemplate "templates/default.html" baseCtx
      >>= relativizeUrls

  -- | Index: Create the index page but using a list of posts
  create ["index.html"] $ do
    route   idRoute
    compile $ do
      -- Get the two latest posts
      posts <- fmap (take 4) $ recentFirst =<< loadAllSnapshots ("posts/*.md" .||. "posts/*.html") "content"
      let ctx = listField "posts" rawPostCtx (return posts) <>
                constField "title" "codetalk"               <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/frontpage.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"   ctx
        >>= relativizeUrls

  -- | Archive: Create the archive page but using a list of the posts
  create ["archive.html"] $ do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("posts/*.md" .||. "posts/*.html") "content"
      let ctx = listField "posts" rawPostCtx (return posts) <>
                constField "title" "Archive"                <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- | Build the tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- | Posts: The post as an HTML page
  match ("posts/*.md" .||. "posts/*.html") $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      -- Create snapshots for later, when using the "raw" blog post
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
      >>= relativizeUrls

  tagsRules tags $ \tag pattern' -> do
    let title = "Posts tagged " ++ tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots pattern' "content"
      let ctx = constField "title" title                                   <>
                constField "currenttag" tag                                <>
                listField "posts" (rawPostWithTagsCtx tags) (return posts) <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-listing.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= relativizeUrls

  -- | Set the pagination grouping to be 3 items, sorted by recent first
  let grouping = fmap (paginateEvery 3) . sortRecentFirst
  -- | Create the identifier for the paginated pages in the format of
  -- 'posts/page/1/index.html' and so on
  let makeIdentifier n = fromFilePath $ "posts/page/" ++ show n
                                        ++ "/index.html"
  -- | Blog pagination: Create the pagination rules for the blog
  blog <- buildPaginateWith grouping ("posts/*.md" .||. "posts/*.html") makeIdentifier

  -- | Blog: Create the paginated blog page
  paginateRules blog $ \pageNum pattern' -> do
    route   idRoute
    compile $ do
      -- Load all the pages from snapshot, to avoid pulling in all the HTML
      posts <- recentFirst =<< loadAllSnapshots pattern' "content"
      let paginateCtx = paginateContext blog pageNum
          ctx = constField "title" "codetalk blog"                         <>
                listField "posts" (rawPostWithTagsCtx tags) (return posts) <>
                paginateCtx                                                <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-listing.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= relativizeUrls
