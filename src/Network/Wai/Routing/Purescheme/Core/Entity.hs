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
{-# LANGUAGE TemplateHaskell #-}
module Network.Wai.Routing.Purescheme.Core.Entity
  ( EntityResponse
  , entity
  , NegotiatedResponse
  , mapEntity
  , withCustomNegotiation
  , withCustomNegotiation'
  , negotiated
  , ok
  , created
  , notFound
  , badRequest
  , entityResponse
  , requestEntity
  )
where

import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Internal

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as LBC
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate.IsString (i)
import Network.HTTP.Media (matchAccept, mapAccept)
import Network.HTTP.Types (Status, ResponseHeaders, notAcceptable406, hAccept, hContentType, 
  statusMessage, badRequest400, unsupportedMediaType415, ok200, created201, notFound404)
import Network.Wai (Response, ResponseReceived, responseLBS, requestHeaders, strictRequestBody)

data EntityResponse e = EntityResponse Status ResponseHeaders e

data NegotiatedResponse = NegotiatedResponse Status ResponseHeaders [(ByteString, LBS.ByteString)]

instance HasResponseHeaders (EntityResponse a) where
  mapResponseHeaders mapf (EntityResponse responseStatus responseHeaders entity) = 
    EntityResponse responseStatus (mapf responseHeaders) entity

instance HasResponseHeaders (NegotiatedResponse) where
  mapResponseHeaders mapf (NegotiatedResponse negotiatedStatus negotiatedHeaders entity) = 
    NegotiatedResponse negotiatedStatus (mapf negotiatedHeaders) entity

-- | Entity Accessor
entity :: EntityResponse e -> e
entity (EntityResponse _ _ e) = e

-- | Maps a entity response
mapEntity :: (a -> b) -> EntityResponse a -> EntityResponse b
mapEntity mapf (EntityResponse responseStatus responseHeaders entity) = 
  EntityResponse responseStatus responseHeaders (mapf entity) 

-- | Converts an application of NegotiatedResponse to a normal WAI Application
-- 
-- This will reject the request with not acceptable (406) in case the content negotation
-- fail
-- 
-- Note: This is going to do the content negotiation after the inner application has repond with 
-- a NegotiatedResponse. That means, any IO is performed before the conetent negoatiation happen.
-- TODO: Find another way to do custom negotiation
-- Better to use @'withCustomNegotiation''
withCustomNegotiation :: GenericApplication NegotiatedResponse -> GenericApplication Response
withCustomNegotiation inner req respond = inner req processNegotiated
  where

    processNegotiated :: NegotiatedResponse -> IO ResponseReceived
    processNegotiated (NegotiatedResponse responseStatus responseHeaders responses) =
      let
        acceptedMediaTypes = fmap fst responses
        respondUsing (mediaType, payload) = 
          let
            newHeaders = addOrReplaceHeader responseHeaders (hContentType, mediaType)
            response = responseLBS responseStatus newHeaders payload
          in respond response
      in
        case lookup hAccept $ requestHeaders req of
          Nothing -> respondUsing $ head responses
          Just "*/*" -> respondUsing $ head responses
          Just accept -> case matchAccept acceptedMediaTypes accept of
            Nothing -> reject' $ notAcceptableRejection acceptedMediaTypes
            Just accepted -> respondUsing $ fromJust $ find (\(k, _) -> k == accepted) responses

-- | The same than @'withCustomNegotiation' but checking the Accept header before doing any IO
withCustomNegotiation' :: [ByteString] -> GenericApplication NegotiatedResponse -> GenericApplication Response
withCustomNegotiation' accepted inner req = 
  let
    doit = withCustomNegotiation inner req
  in
    case lookup hAccept $ requestHeaders req of 
      Nothing -> doit
      Just "*/*" -> doit
      Just accept -> case matchAccept accepted accept of
        Nothing -> reject $ notAcceptableRejection accepted
        Just _ -> doit

notAcceptableRejection :: [ByteString] -> Rejection
notAcceptableRejection acceptedResponses = 
  Rejection
    { status = notAcceptable406
    , message = [i|#{statusMessage notAcceptable406}: Acceptable media types: #{LBC.intercalate ", " acceptedResponses}|]
    , priority = 200
    , headers = []
    }

-- | Converts a entity response to a negotiated entity
negotiated :: [(ByteString, a -> LBS.ByteString)] -> EntityResponse a ->  NegotiatedResponse
negotiated accptableResponses (EntityResponse responseStatus responseHeaders entity) = 
  NegotiatedResponse responseStatus responseHeaders (fmap (\(key, v) -> (key, v entity)) accptableResponses)
  -- TODO: This uses lazyness from haskell so that not all payloads are generated

-- | Reads the entity and pass it to provided function
--
-- The map provides the accepted media types with the functions that decodes it
-- 
-- As specified in https://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
-- a missing content type header from the request is treated "application/octet-stream"
--
-- Note: This will read all the payload in memory and then decode it
-- so it can blow up the memory. Better to have a guard on the size of the request
requestEntity :: [(ByteString, LBS.ByteString -> Either String a)] -> (a -> GenericApplication b) -> GenericApplication b
requestEntity mappings fa req respond = 
  let 
    contentTypeHeader = fromMaybe "application/octet-stream" $ lookup hContentType $ requestHeaders req
  in
    case mapAccept mappings contentTypeHeader of
      Just decodeFunc -> do
        decodedOrError <- decodeFunc <$> strictRequestBody req
        case decodedOrError of
          Left decodeError -> reject' $ decodeErrorRejection decodeError
          Right decoded -> fa decoded req respond
      Nothing -> reject' $ unsupportedMediaTypeRejection $ fmap fst mappings

decodeErrorRejection :: String -> Rejection
decodeErrorRejection reason = 
  Rejection
    { status = badRequest400
    , message = [i|#{statusMessage badRequest400}: Error reading entity: #{reason}|]
    , priority = 200
    , headers = []
    }

unsupportedMediaTypeRejection :: [ByteString] -> Rejection
unsupportedMediaTypeRejection supportedMediaTypes = 
  Rejection
    { status = unsupportedMediaType415
    , message = [i|#{statusMessage unsupportedMediaType415}: Supported Media Types: #{LBC.intercalate ", " supportedMediaTypes}|]
    , priority = 200
    , headers = []
    }

-- | Creates an entity response with status 200
ok :: a -> EntityResponse a
ok = EntityResponse ok200 []

-- | Creates a entity response with status 201
created :: a -> EntityResponse a
created = EntityResponse created201 []

-- | Creates an entity response with status 404
notFound :: a -> EntityResponse a
notFound = EntityResponse notFound404 []

-- | Creates an entity response with status 400
badRequest :: a -> EntityResponse a
badRequest = EntityResponse badRequest400 []

-- | Creates a entity response with the provided status and response headers
entityResponse :: Status -> ResponseHeaders -> a -> EntityResponse a
entityResponse = EntityResponse