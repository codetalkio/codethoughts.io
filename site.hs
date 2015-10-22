{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
-- For compressJsCompiler
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Text.Jasmine


compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap jasmin <$> getResourceString

jasmin :: String -> String
jasmin src = LB.unpack $ minify $ LB.fromChunks [E.encodeUtf8 $ T.pack src]

-- | Context for posts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- | Context for posts
frontpagePostCtx :: Context String
frontpagePostCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "frontpage" "yes" `mappend`
    defaultContext

-- | Main defines all the route handling
main :: IO ()
main = hakyll $ do
    -- | Route for all images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- | Compile SCSS to CSS and serve it
    match "scss/app.scss" $ do
        route   $ constRoute "app.css"
        compile $ liftM (fmap compressCss) $
            getResourceString
            >>= withItemBody (unixFilter "sass" [ "-s"
                                                , "--scss"
                                                , "--compass"
                                                , "--style", "compressed"
                                                , "--load-path", "scss"
                                                ])

    -- | Route for all javascript files
    match "js/*" $ do
        route   idRoute
        compile compressJsCompiler

    -- | Load all partial templates
    match "templates/*" $ compile templateCompiler

    -- | Pages: Load all standard pages using the page template
    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- | Posts: All posts use the post template
    match "posts/*" $ version "source" $ do
        route $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" frontpagePostCtx
            >>= relativizeUrls

    -- | Posts: All posts use the post template
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- | Index/Blog: Create the index page but using a list of the posts
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "source")
            let blogCtx =
                    listField "posts" frontpagePostCtx (return posts) `mappend`
                    constField "title" "Blog"                         `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls
