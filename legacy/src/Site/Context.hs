{-# LANGUAGE OverloadedStrings #-}

module Site.Context where

import Control.Applicative (empty)
import Data.String (fromString)
import Data.Text (pack, unpack)
import Hakyll
import Text.Pandoc

-- | Base context, making common items available
baseCtx :: Context String
baseCtx =
  dateField "date" "%e. %B %Y" <> dateField "prettydate" "%B %e, %Y" <>
  dateField "datetime" "%Y-%e-%b" <>
  constField "blogurl" "/posts/page/1/index.html" <>
  pathField "urlencodedtitle" <>
  defaultContext

-- | Context for normal standalone posts
postCtx :: Tags -> Context String
postCtx tags =
  constField "isNormalPost" "yes" <>
  tagsField "tags" tags <>
  versionsCtx <>
  baseCtx

-- | Context for raw posts (i.e. only the post body)
rawPostCtx :: Context String
rawPostCtx =
  constField "isRawPost" "yes" <>
  baseCtx

-- | Context for raw posts with tags
rawPostWithTagsCtx :: Tags -> Context String
rawPostWithTagsCtx tags =
  tagsField "tags" tags <>
  rawPostCtx

-- | Expose the blog post teaser part in the context.
teaserCtx :: Tags -> Context String
teaserCtx tags =
  teaserField "teaser" "raw-post-content" <>
  rawPostWithTagsCtx tags


-- | Convert versions metadata, if present, into a list.
versionsCtx :: Context String
versionsCtx =
  listFieldWith "versions" ctx renderVersions <>
  -- We create a hasVersions, because checking on $versions$ in templates does not work.
  boolFieldM "hasVersions" hasVersions
  where
    ctx = field "version" (pure . itemBody)

    mVersions item = do
      -- Extract the meta data field into a list of strings.
      metadata <- getMetadata (itemIdentifier item)
      pure $ lookupStringList "versions" metadata

    hasVersions item = do
      versions <- mVersions item
      pure $ if length versions > 0 then True else False

    renderVersions item = do
      versions <- mVersions item
      -- Convert the list of unrendered markdown into rendered HTML.
      case versions of
        Just lst -> pure $ fmap mkVersionItem lst
        Nothing     -> pure []

    mkVersionItem v =
      Item {
        itemIdentifier = fromString ("version/" ++ v),
        itemBody = case renderMarkdownString v of
          Left _ -> ""
          Right s -> s
      }

-- | Convert markdown string into rendered HTML.
renderMarkdownString :: String -> Either PandocError String
renderMarkdownString v = runPure $ do
      m <- readMarkdown def (pack v)
      i <- writeHtml5String def m
      pure $ unpack i

-- | Like 'boolField' but allows the use of the 'Compiler' monad.
-- | Taken from the open PR at https://github.com/jaspervdj/hakyll/pull/565.
boolFieldM :: String -> (Item a -> Compiler Bool) -> Context a
boolFieldM name f = field name $ \i -> do
  b <- f i
  if b
    then pure $ error $ unwords $
            ["no string value for bool field:", name]
    else empty
