{-# LANGUAGE OverloadedStrings #-}

module Site.Context where

import Data.Monoid ((<>))
import Hakyll

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
  constField "isNormalPost" "yes" <> tagsField "tags" tags <> baseCtx

-- | Context for raw posts (i.e. only the post body)
rawPostCtx :: Context String
rawPostCtx = constField "isRawPost" "yes" <> baseCtx

-- | Context for raw posts with tags
rawPostWithTagsCtx :: Tags -> Context String
rawPostWithTagsCtx tags = tagsField "tags" tags <> rawPostCtx
