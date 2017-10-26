{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text         (Text, append, drop, isPrefixOf, pack,
                                    toLower, unpack)
import           GHC.Generics
import           Prelude           hiding (drop)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

data HaveRead = HaveRead { hrTotal      :: Int
                         , hrHaskell    :: Int
                         , hrHackerNews :: Int
                         , hrOther      :: Int
                         , hrLinks      :: [Link]
                         , hrMonth      :: String
                         , hrYear       :: String
                         } deriving (Show, Generic)

data Link = Link { bmTitle :: Text
                 , bmLink  :: Text
                 } deriving (Show, Generic)

cleanUp :: String -> String
cleanUp s = unpack $ toLower $ drop 2 (pack s)

instance ToJSON Link where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = cleanUp }
instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = cleanUp }

-- http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html
-- https://fateswanderer.wordpress.com/2013/11/17/parsing-with-handsomesoup/
main :: IO ()
main = do
  bmJan <- readBookmarks "January"
  print $ generateReadingList "January" "2016" bmJan

generateReadingList :: String -> String -> [Link] -> HaveRead
generateReadingList month year links = do
  let haskellLinks = filterDomain "reddit.com/r/haskell" links
  let hackerNewsLinks = filterDomain "news.ycombinator.com" links
  let otherLinks = length links - length haskellLinks - length hackerNewsLinks
  HaveRead { hrTotal = length links
           , hrHaskell = length haskellLinks
           , hrHackerNews = length hackerNewsLinks
           , hrOther = otherLinks
           , hrLinks = [] -- links
           , hrMonth = month
           , hrYear = year
           }

-- | Filter a list of links based on the domain, handling 'http', 'https' and
-- 'www' prefixes to be present
filterDomain :: Text -> [Link] -> [Link]
filterDomain domain = filter (hasDomain . bmLink)
  where
    hasDomain :: Text -> Bool
    hasDomain l = isPrefixOf (append "http://" domain) l
               || isPrefixOf (append "http://www." domain) l
               || isPrefixOf (append "https://" domain) l
               || isPrefixOf (append "https://www." domain) l

-- | Get all the links from the bookmark file as a (title, link) pair
readBookmarks :: String -> IO [Link]
readBookmarks month = do
  -- Read the bookmarks file
  bookmarks <- readFile $ "/Users/tehnix/Desktop/" ++ month ++ ".html"
  let doc = readString [withParseHTML yes, withWarnings no] bookmarks
  -- Parse the HTML for all links and their href attributes
  links <- runX $ doc >>> css "a" >>> getAttrValue "href" &&& deep getText
  -- Convert the list of (String, String) to the Link datatype
  return $ map (\(l,t) -> Link {bmTitle = pack t, bmLink = pack l}) links
