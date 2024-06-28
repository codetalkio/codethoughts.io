{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens (mapped, (%~), (&), (.~), (^.))

import Data
import Data.Aeson (decode, encode)


main :: IO ()
main = do
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
  -- You can usually also use `traverse` instead of `mapped` here.
  print $ house & #people . mapped %~ #firstname %~ ("Fly " <>)

  -- ## Encode / Serialize.
  putStrLn "\n\n## Encode / Serialize."
  print $ encode house

  -- ## Decode / Deserialize.
  putStrLn "\n\n## Decode / Deserialize."
  let houseJson = encode house
  print $ decode @Household houseJson
