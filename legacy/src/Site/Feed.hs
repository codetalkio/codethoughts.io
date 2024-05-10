{-# LANGUAGE OverloadedStrings #-}

module Site.Feed where

import Hakyll.Web.Feed (FeedConfiguration(..))

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Tech blog: Latest Posts"
    , feedDescription = "Follow the latest posts on tech, programming and beyond!"
    , feedAuthorName  = "Christian Kjær Laustsen"
    , feedAuthorEmail = "ckl@codetalk.io"
    , feedRoot        = "https://codetalk.io"
    }
