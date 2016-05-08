{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
-- For SASS/SCSS compression
import Control.Monad (liftM)
-- For compressJsCompiler
import qualified Data.ByteString.Lazy.Char8 as C
import           Text.Jasmine


minifyJSCompiler = do
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

minifyJS = C.unpack . minify . C.pack . itemBody

-- | Context for posts
postCtx :: Context String
postCtx =
    dateField "date" "%e. %B %Y" `mappend`
    dateField "prettydate" "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%e-%b" `mappend`
    defaultContext

-- | Context for raw posts (i.e. only the post body)
rawPostCtx :: Context String
rawPostCtx =
    constField "rawPost" "yes" `mappend`
    postCtx

main :: IO ()
main = hakyll $ do
    -- | Route for all images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- | Route for all CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- | Route for all JavaScript files
    match "js/*" $ do
        route idRoute
        compile minifyJSCompiler

    -- | Compile SCSS to CSS and serve it
    match "scss/app.scss" $ do
       route   $ constRoute "app.css"
       compile $ fmap (fmap compressCss) $
           getResourceString
           >>= withItemBody (unixFilter "sass" [ "-s"
                                               , "--scss"
                                               , "--compass"
                                               , "--style", "compressed"
                                               , "--load-path", "scss"
                                               ])

    -- | Load all partial templates
    match "templates/*" $ compile templateCompiler

    -- | Pages: Load all standard pages using the page template
    match "pages/*" $ version "markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- | Posts: Only the content of the post itself
    match "posts/*" $ version "source" $ do
        route $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" rawPostCtx
            >>= relativizeUrls

    -- | Posts: The post as an HTML page
    match "posts/*" $ version "markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- | Archive: Create the archive page but using a list of the posts
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "markdown")
            let archiveCtx =
                    listField "posts" rawPostCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- | Blog: Create the blog page using a list of the posts
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "source")
            let blogCtx =
                    listField "posts" rawPostCtx (return posts) `mappend`
                    constField "title" "Blog"                         `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    -- | index: Create the index page but using a list of posts
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- do
                ps  <- loadAll ("posts/*" .&&. hasVersion "source")
                rps <- recentFirst ps
                -- Only take the two latest post
                return $ take 2 rps
            let blogCtx =
                    listField "posts" rawPostCtx (return posts) `mappend`
                    constField "title" "codetalk"               `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/home.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls
