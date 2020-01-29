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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Wai.Routing.Purescheme.Core.Entity.Json
  ( withContentNegotiationJson
  , entityJson
  )
where

import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Entity
import Network.Wai.Routing.Purescheme.Core.Internal

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import Network.HTTP.Media (MediaType, matchContent)
import Network.HTTP.Types (unsupportedMediaType415, badRequest400, statusMessage, hContentType)
import Network.Wai (Response, strictRequestBody, requestHeaders)

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)

withContentNegotiationJson :: ((forall a. ToJSON a => EntityResponse a -> NegotiatedResponse) -> GenericApplication NegotiatedResponse) -> GenericApplication Response
withContentNegotiationJson f = withCustomNegotiation $ f $ negotiated negotiationJson

negotiationJson :: ToJSON a => [(ByteString, a -> LBS.ByteString)]
negotiationJson = 
  [ ("application/json", encode)
  ]


-- TODO: We are doing strictRequestBody that means, we are going to read all the body in memory
-- so that we need to have a guard on the bytes of the content (Content-Length)
entityJson :: FromJSON a => (a -> GenericApplication b) -> GenericApplication b
entityJson inner req respond = 
  if isValidContentType
    then do
      valueString <- strictRequestBody req
      case eitherDecode valueString of
        Right result -> inner result req respond
        Left decodeError -> reject' $ decodeErrorRejection $ T.pack decodeError
    else reject' $ unsupportedMediaTypeRejection "Content-Type not supported"
  where
    isValidContentType = 
      case lookup hContentType $ requestHeaders req of 
        Nothing -> True
        Just contentTypeHeader -> case matchContent ["application/json" :: MediaType] contentTypeHeader of
          Nothing -> False
          Just _ -> True

unsupportedMediaTypeRejection :: T.Text -> Rejection
unsupportedMediaTypeRejection errorMessage =
  Rejection
    { status = unsupportedMediaType415
    , message = [i|#{statusMessage unsupportedMediaType415}: #{errorMessage}|]
    , priority = 200
    , headers = []
    }

decodeErrorRejection :: T.Text -> Rejection
decodeErrorRejection errorMessage =
  Rejection
    { status = badRequest400
    , message = [i|#{statusMessage badRequest400}: Error decoding entity body: #{errorMessage}|]
    , priority = 300
    , headers = []
    }

