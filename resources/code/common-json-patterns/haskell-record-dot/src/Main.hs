{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (id)

import Data
import Data.Aeson (encode, decode)


main :: IO ()
main = do
  -- ## Get a field.
  putStrLn "\n\n## Get a field."
  print $ house.owner

  -- ## Get a nested field.
  putStrLn "\n\n## Get a nested field."
  print $ house.owner.firstname
  -- print $ house ^. #owner . #firstname

  -- ## Get an optional field.
  putStrLn "\n\n## Get an optional field."
  putStrLn "Return the value in a Maybe:"
  print $ house.address

  putStrLn "\nA field on an object that exists:"
  print $ maybe "" (.address) house.address

  putStrLn "\nA field on an object that does *NOT* exist (falls back to an empty value):"
  print $ maybe "" (.address) house.alternativeAddress

  -- ## Update a field.
  putStrLn "\n\n## Update a field."
  let newAriel = Person { id = 4, firstname = "New Ariel", lastname = "Swanson" }
  print $ house{ owner = newAriel}

  -- ## Update a nested field.
  putStrLn "\n\n## Update a nested field."
  print $ house{ owner.firstname = "New Ariel"}

  -- ## Update each item in a list.
  putStrLn "\n\n## Update each item in a list."
  print $ house{ people = map (\p -> p{firstname = "Fly " ++ p.firstname}) house.people}

  -- ## Encode / Serialize.
  putStrLn "\n\n## Encode / Serialize."
  print $ encode house

  -- ## Decode / Deserialize.
  putStrLn "\n\n## Decode / Deserialize."
  let houseJson = encode house
  print $ decode @Household houseJson
