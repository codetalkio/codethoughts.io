{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON(..), Generic, OmitNothingFields)
import Prelude hiding (id)

data Address = Address
  { country :: String
  , address :: String
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] Address

data Person = Person
  { id :: Int
  , firstname :: String
  , lastname :: String
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] Person

data Household = Household
  { id :: Int
  , people :: [Person]
  , address :: Maybe Address
  , alternativeAddress :: Maybe Address
  , owner :: Person
  } deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[OmitNothingFields] Household

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
