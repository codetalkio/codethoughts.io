{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Lens ((^.))

import Data


main :: IO ()
main = do
  -- Showcase our derived JSON object.
  putStrLn "\n\nShowcase our derived JSON object:"
  encodeJson house

  -- ## Get a field.
  putStrLn "\n\n## Get a field."
  print $ house ^. #owner

  -- ## Get a nested field.
  putStrLn "\n\n## Get a nested field."
  print $ house ^. #owner . #firstname

  -- ## Get an optional field.
  putStrLn "\n\n## Get an optional field."
  putStrLn "Return the value in a Maybe:"
  print $ house ^. #address

  putStrLn "A field on an object that exists:"
  print $ house ^. #address . #_Just . #address

  putStrLn "A field on an object that does *NOT* exist (falls back to an empty value):"
  print $ house ^. #alternativeAddress . #_Just . #address

  -- ## Update a field.


  -- ## Update a nested field.


  -- ## Update each item in a list.


  -- -- Accessors are more powerful:
  -- print $ foo ^? #moarStuff . #_Just . #name
  -- print $ foo ^? #moarStuff . #_Just . #moarStuff

  -- -- Setters:
  -- print $ foo & #count %~ (+ 1)
  -- print $ foo & #name .~ "bar"
  -- print $ foo & #moarStuff . #_Just . #count %~ (+ 1)
  -- -- and much much more!
