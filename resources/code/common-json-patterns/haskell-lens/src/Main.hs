{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Lens (mapped, (%~), (&), (.~), (^.))

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

  putStrLn "\nA field on an object that exists:"
  print $ house ^. #address . #_Just . #address

  putStrLn "\nA field on an object that does *NOT* exist (falls back to an empty value):"
  print $ house ^. #alternativeAddress . #_Just . #address

  -- ## Update a field.
  putStrLn "\n\n## Update a field."
  let newAriel = Person { id = 4, firstname = "New Ariel", lastname = "Swanson" }
  print $ house & #owner .~ newAriel

  -- ## Update a nested field.
  putStrLn "\n\n## Update a nested field."
  print $ house & #owner . #firstname .~ "New Ariel"

  -- ## Update each item in a list.
  putStrLn "\n\n## Update each item in a list."
  print $ house & #people . mapped %~ (\p -> p & #firstname .~ "Fly " ++ p ^. #firstname)

  -- -- Accessors are more powerful:
  -- print $ foo ^? #moarStuff . #_Just . #name
  -- print $ foo ^? #moarStuff . #_Just . #moarStuff

  -- -- Setters:
  -- print $ foo & #count %~ (+ 1)
  -- print $ foo & #name .~ "bar"
  -- print $ foo & #moarStuff . #_Just . #count %~ (+ 1)
  -- -- and much much more!
