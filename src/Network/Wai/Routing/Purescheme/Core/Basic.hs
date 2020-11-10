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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Wai.Routing.Purescheme.Core.Basic
  ( GenericApplication
  , Rejection(..)
  , FromUri(..)
  , HasResponseHeaders(..)
  , alternatives
  , handleException
  , withDefaultExceptionHandler
  , complete
  , completeIO
  , mapResponse
  , withRequest
  , withIO
  )
where

import Network.Wai.Routing.Purescheme.Core.Internal

import Control.Exception (Exception, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (ResponseHeaders, hContentType)
import Network.Wai (Response, ResponseReceived, Request, responseLBS)
import qualified Network.Wai as Wai

-- | Abstraction of Wai @'Application' on the type of response
type GenericApplication r = Request -> (r -> IO ResponseReceived) -> IO ResponseReceived

-- | Class of types that can be converted from the uri
class FromUri a where
  fromText :: T.Text -> a
  fromByteString :: ByteString -> a
  fromByteString = fromText . T.decodeUtf8

instance FromUri T.Text where
  fromText = id

instance FromUri Bool where
  fromText p = read $ T.unpack p

instance FromUri Int where
  fromText p = read $ T.unpack p

instance FromUri Int32 where
  fromText p = read $ T.unpack p

instance FromUri Int64 where
  fromText p = read $ T.unpack p

instance FromUri LT.Text where
  fromText = LT.fromStrict

-- | Class which instaances contains response heaaders
class HasResponseHeaders a where
  mapResponseHeaders :: (ResponseHeaders -> ResponseHeaders) -> a -> a

instance HasResponseHeaders Response where
  mapResponseHeaders = Wai.mapResponseHeaders

-- | Combines multiple generic applications in one
-- This function will try every application for each request, and return the first
-- response that does not fail
--
-- In case of rejections (Reection thrown), it will rethrown the first
-- exception with higher priority
alternatives :: [GenericApplication r] -> GenericApplication r
alternatives = alternatives' notFoundDefaultRejection
  where
    alternatives' :: Rejection -> [GenericApplication r] -> GenericApplication r
    alternatives' rejection [] _ _ = reject' rejection
    alternatives' rejection (x:xs) req respond =
      x req respond `catch` \e -> alternatives' (chooseRejection rejection e) xs req respond

    chooseRejection r1 r2 = 
      if priority r1 < priority r2
        then r2
        else r1

-- Exception Handler functions

-- | Capture exceptions and convert to generic applications
handleException :: Exception e => (e -> GenericApplication a) -> GenericApplication a -> GenericApplication a
handleException exceptionFunc innerApp req resp = 
  catch (innerApp req resp) (\e -> exceptionFunc e req resp)

-- | By default capture all @'Rejection' and convert them in specific responses
-- the content type returned is 'text/plain" and the body will contain the error message
withDefaultExceptionHandler :: GenericApplication Response -> GenericApplication Response
withDefaultExceptionHandler = handleException handleRejection
  where
    handleRejection :: Rejection -> GenericApplication Response
    handleRejection Rejection{status, message} _ respond = 
      respond $ responseLBS status [(hContentType, "text/plain")] (LBS.fromStrict $ T.encodeUtf8 message)

-- | Ends the request responding with the argument
complete :: a -> GenericApplication a
complete response _ respond = respond response

-- | Ends the request excuting the provided IO and responding the result of the IO
completeIO :: IO a -> GenericApplication a
completeIO responseIO _ respond = do
  response <- responseIO
  respond response

-- | Execute an IO Action and pass it to the provided function
withIO :: IO a -> (a -> GenericApplication b) -> GenericApplication b
withIO theIO f req respond = do
  var <- theIO
  f var req respond

-- | Maps a response type to another response type
mapResponse :: (a -> b) -> GenericApplication a -> GenericApplication b
mapResponse mapf inner req respond = inner req (respond . mapf)

-- | Pass the request to the provided function
withRequest :: (Request -> GenericApplication a) -> GenericApplication a
withRequest reqFun req = reqFun req req
