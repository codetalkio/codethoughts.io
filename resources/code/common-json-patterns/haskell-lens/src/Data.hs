{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}

module Data where

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as ByteLazy
import Data.Generics.Labels ()
import Deriving.Aeson (CustomJSON(..), Generic)

data Address = Address
  { country :: String
  , address :: String
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
  via CustomJSON '[] Address

data Person = Person
  { id :: Int
  , firstname :: String
  , lastname :: String
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
  via CustomJSON '[] Person

data Household = Household
  { id :: Int
  , people :: [Person]
  , address :: Maybe Address
  , alternativeAddress :: Maybe Address
  , owner :: Person
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
  via CustomJSON '[] Household

-- | The object we will be working with througout our examples.
house :: Household
house = Household
  { id = 1
  , people = [mom, dad, son]
  , address = Just addr
  , alternativeAddress = Nothing
  , owner = mom
  }
  where
    addr = Address { country = "Ocean", address = "Under the sea" }
    mom = Person { id = 1, firstname = "Ariel", lastname = "Swanson" }
    dad = Person { id = 2, firstname = "Triton", lastname = "Swanson" }
    son = Person { id = 3, firstname = "Eric", lastname = "Swanson" }

-- | Helper function for printing out our data types as JSON objects.
encodeJson :: ToJSON a => a -> IO ()
encodeJson item = ByteLazy.putStrLn $ encode item
