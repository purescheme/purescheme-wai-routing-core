-- Copyright 2020 Fernando Rincon Martin
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Logic where

import Prelude hiding (id)
import Data.Aeson
import Data.IORef (IORef, readIORef, newIORef, atomicModifyIORef')
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics

type Context = IORef [Pet]

data Pet = 
  Pet
  { id :: Int
  , name :: Text
  , tag :: Text
  } deriving (Generic, Show)

instance ToJSON Pet
instance FromJSON Pet

data OffsetPagination = 
  OffsetPagination
  { limit :: Maybe Int
  , page :: Maybe Int
  }
  
data ListResult a = ListResult
  { resultList :: [a]
  , page :: Int
  , totalPages :: Int
  , itemsPerPage :: Int
  , itemsTotal :: Int
  }

-- Model/DAO Methods
initialContext :: IO (IORef [Pet])
initialContext = newIORef 
  [ Pet 1 "scooby" "no-tag"
  , Pet 2 "canelo" "dog"
  ]

defaultLimit :: Int
defaultLimit = 10

listPets :: Context -> OffsetPagination -> IO (ListResult Pet)
listPets context OffsetPagination{..} = do
  let actualLimit = fromMaybe defaultLimit limit
  let actualPage = fromMaybe 1 page
  pets <- readIORef context
  return ListResult
    { resultList = take actualLimit $ drop ((actualPage - 1) * actualLimit) pets
    , page = actualPage
    , totalPages = ((length pets) `div` actualLimit) + if (length pets) `mod` actualLimit == 0 then 0 else 1
    , itemsPerPage = actualLimit
    , itemsTotal = length pets
    }

getPet :: Context -> Int -> IO (Maybe Pet)
getPet context petId = do
  pets <- readIORef context
  return $ find (\pet -> id pet == petId) pets

createOrReplacePet :: Context -> Pet -> IO Bool
createOrReplacePet context newPet = atomicModifyIORef' context doIt
  where
    doIt pets = 
      ( newPet:(filter (\pet -> id pet /= id newPet) pets)
      , all (\pet -> id pet /= id newPet) pets
      )
