{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Hakyll

import Site.Compiler (sassCompiler)
import Site.Context (baseCtx, postCtx, rawPostCtx, teaserCtx)
import Site.Feed (feedConfiguration)

postPatterns :: Pattern
postPatterns = "posts/*.md" .||. "posts/*.html"

-- | Define the rules for the site/hakyll compiler
main :: IO ()
main = hakyll $ do
  -- | Route for all images
  match "resources/images/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- | Route for all pdfs
  match "resources/pdfs/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- | Route for the favicon
  match "resources/favicon.ico" $ do
    route   $ constRoute "favicon.ico"
    compile copyFileCompiler

  -- | Route for the rest of the favicons
  match "resources/favicon/*" $ do
    route   idRoute
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
      posts <- fmap (take 7) $ recentFirst =<< loadAllSnapshots postPatterns "raw-post-content"
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
      posts <- recentFirst =<< loadAllSnapshots postPatterns "raw-post-content"
      let ctx = listField "posts" rawPostCtx (return posts) <>
                constField "title" "Archive"                <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- | Build the tags
  tags <- buildTags postPatterns (fromCapture "tags/*.html")

  -- | Create an RSS and Atom feed
  let feedContent = do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postPatterns "processed-post-content"
            renderAtom feedConfiguration feedCtx posts
  create ["atom.xml"] feedContent
  create ["rss.xml"] feedContent

  -- | Posts: The post as an HTML page
  match postPatterns $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      -- Create snapshots of the raw post, before any templates are applied. We can get this
      -- later by loading the snapshot from `raw-post-content`.
      >>= saveSnapshot "raw-post-content"
      >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
      -- Create snapshots of the processed post, before any the final templates is applied. We can get this
      -- later by loading the snapshot from `postContent`.
      >>= saveSnapshot "processed-post-content"
      >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
      >>= relativizeUrls

  tagsRules tags $ \tag pattern' -> do
    let title = "Posts tagged " ++ tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots pattern' "raw-post-content"
      let ctx = constField "title" title                                   <>
                constField "currenttag" tag                                <>
                listField "posts" (teaserCtx tags) (return posts) <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-listing.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= relativizeUrls

  -- | Set the pagination grouping to be 3 items, sorted by recent first
  let grouping = fmap (paginateEvery 10) . sortRecentFirst
  -- | Create the identifier for the paginated pages in the format of
  -- 'posts/page/1/index.html' and so on
  let makeIdentifier n = fromFilePath $ "posts/page/" ++ show n
                                        ++ "/index.html"
  -- | Blog pagination: Create the pagination rules for the blog
  blog <- buildPaginateWith grouping postPatterns makeIdentifier

  -- | Blog: Create the paginated blog page
  paginateRules blog $ \pageNum pattern' -> do
    route   idRoute
    compile $ do
      -- Load all the pages from snapshot, to avoid pulling in all the HTML
      posts <- recentFirst =<< loadAllSnapshots pattern' "raw-post-content"
      let paginateCtx = paginateContext blog pageNum
          ctx = constField "title" "codetalk blog"                         <>
                listField "posts" (teaserCtx tags) (return posts) <>
                paginateCtx                                                <>
                baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-listing.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= relativizeUrls
