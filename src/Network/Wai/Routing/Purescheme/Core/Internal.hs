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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Network.Wai.Routing.Purescheme.Core.Internal (
    Rejection(..)
  , reject
  , reject'
  , notFoundDefaultRejection
  , addOrReplaceHeader
) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Network.Wai (ResponseReceived)
import Network.HTTP.Types (Status, ResponseHeaders, Header, notFound404, statusMessage)

data Rejection 
  = Rejection
  { message :: Text
  , priority :: Int
  , status :: Status
  , headers :: ResponseHeaders
  } 
  deriving (Show, Typeable)

instance Exception Rejection

reject :: Rejection -> (r -> IO ResponseReceived) -> IO ResponseReceived
reject rejectionException _ = reject' rejectionException

reject' :: Rejection -> IO ResponseReceived
reject' = throwIO

addOrReplaceHeader :: [Header] -> Header -> [Header]
addOrReplaceHeader fromHeaders header@(key, _) = 
   header:filter (\(k, _) -> k /= key) fromHeaders

notFoundDefaultRejection :: Rejection
notFoundDefaultRejection = 
  Rejection
    { status = notFound404
    , message = decodeUtf8 $ statusMessage notFound404
    , priority = minBound
    , headers = []
    }