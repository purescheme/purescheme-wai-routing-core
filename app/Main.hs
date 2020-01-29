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
module Main where

import Logic

import Network.Wai.Routing.Purescheme.Core

import Prelude hiding (id)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types (status200, StdMethod(..), Header, hContentType, statusCode)
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseLBS, Response, Request, Application)

data ErrorResponse =
  ErrorResponse
  { code :: Int
  , message :: Text
  } deriving (Generic, Show)

instance ToJSON ErrorResponse

main :: IO ()
main = do
  context <- initialContext
  run 8080 $ api context

-- The API

api :: Context -> Application
api context = handleException handleRejections $ alternatives [restApiV1, v1 context]
  
handleRejections :: Rejection -> Application
handleRejections Rejection{..} = 
  withContentNegotiationJson $ \negotiate ->
    complete $ negotiate $ entityResponse status headers $ ErrorResponse (statusCode status) message

-- Simple Api
restApiV1 :: Application
restApiV1 = path "hello"
            $ method GET
            $ complete
            $ responseLBS status200 [(hContentType, "text/html")] "<h1>Hellow World!</h1>"

v1 :: Context -> Application
v1 context =   
  pathSegment "v1" $ alternatives 
    [ pathSegment "pets" $ alternatives 
      [ pathEnd $ method GET $ listPetsEndpointV1 context
      , pathVar $ \petId -> pathEnd $ alternatives 
        [ method GET $ showPetByIdEndpointV1 context petId
        , method PUT $ createOrUpdatePetEndpointV1 context petId
        ]
      ]
    ]

listPetsEndpointV1 :: Context -> GenericApplication Response
listPetsEndpointV1 context = 
  withContentNegotiationJson $ \negotiate ->
    mapResponse negotiate $
      withOffsetPagination $ \pagination -> 
        completeIO $ ok <$> listPets context pagination

showPetByIdEndpointV1 :: Context -> Int -> GenericApplication Response
showPetByIdEndpointV1 context petId =
  withContentNegotiationJson $ \negotiate -> completeIO $ do
    maybePet <- getPet context petId
    return $ case maybePet of
      Nothing -> negotiate $ notFound $ ErrorResponse 404 "Pet nof found"
      Just pet -> negotiate $ ok pet

createOrUpdatePetEndpointV1 :: Context -> Int -> GenericApplication Response
createOrUpdatePetEndpointV1 context petId =
  entityJson $ \pet ->
    withContentNegotiationJson $ \negotiate -> completeIO $
      if petId /= id pet
        then return $ negotiate $ badRequest $ ErrorResponse 400 "Pet id does not match with the URL"
        else do
          isNew <- createOrReplacePet context pet
          if isNew
            then return $ negotiate $ created pet
            else return $ negotiate $ ok pet

-- Common abstraction for offset based pagination

withOffsetPagination :: (OffsetPagination -> GenericApplication (EntityResponse (ListResult a))) -> GenericApplication (EntityResponse [a])
withOffsetPagination f = 
  maybeSingleParameter "limit" $ \mLimit -> 
  maybeSingleParameter "page" $ \mPage ->
    withRequest $ \req ->  
      mapResponse (transformResponse req) $ f $ OffsetPagination { limit = mLimit, page = mPage }

  where

      transformResponse :: Request -> EntityResponse (ListResult a) -> EntityResponse [a]
      transformResponse req resp = mapResponseHeaders insertLinkHeader $ mapEntity resultList resp
      
        where

          listResult = entity resp

          insertLinkHeader :: [Header] -> [Header]
          insertLinkHeader headers = ("link", linkHeaderValue):filter (\(k, _) -> k /= "link" ) headers

          linkHeaderValue :: BS.ByteString
          linkHeaderValue = BS.intercalate "," $ catMaybes [nextHeader, lastHeader, firstHeader, prevHeader]

          -- TODO Make Link headers
          nextHeader = Nothing
          lastHeader = Nothing
          firstHeader = Nothing
          prevHeader = Nothing

{- 
-- XmlAndJson Support

-- V2 Api:
-- Supports JSON and XML

v2 :: Context -> Application
v2 context = 
  pathSegment "v2" $ alternatives 
    [ pathSegment "pets" $ alternatives 
      [ pathEnd $ method GET $ listPetsEndpointV1 context
      , pathVar $ \petId -> pathEnd $ alternatives
        [ method GET $ showPetByIdEndpointV1 context petId
        , method PUT $ createOrUpdatePetEndpointV1 context petId
        ]
      ]
    ]

listPetsEndpointV2 :: Context -> GenericApplication Response
listPetsEndpointV2 context = 
  withXmlAndJson $ \negotiate ->
    mapResponse negotiate $
      withOffsetPagination $ \pagination -> 
        completeIO $ ok <$> listPets context pagination

showPetByIdEndpointV2 :: Context -> Int -> GenericApplication Response
showPetByIdEndpointV2 context petId =
  withXmlAndJson $ \negotiate -> completeIO $ do
    maybePet <- getPet context petId
    return $ case maybePet of
      Nothing -> negotiate $ notFound $ ErrorResponse 404 "Pet nof found"
      Just pet -> negotiate $ ok pet

createOrUpdatePetEndpointV2 :: Context -> Int -> GenericApplication Response
createOrUpdatePetEndpointV2 context petId =
  requestEntity jsonOrXml $ \pet ->
    withXmlAndJson $ \negotiate -> completeIO $
      if petId /= id pet
        then return $ negotiate $ badRequest $ ErrorResponse 400 "Pet id does not match with the URL"
        else do
          isNew <- createOrReplacePet context pet
          if isNew
            then return $ negotiate $ created pet
            else return $ negotiate $ ok pet

jsonOrXml :: (FromJSON a, FromXML a) => [(BS.ByteString, ByteString -> Either String a)]
jsonOrXml = 
  [ ("application/json", eitherDecode)
  , ("application/xml", fromXML)
  ]



withXmlAndJson :: ((forall a. (ToJSON a, ToXML a) => EntityResponse a -> NegotiatedResponse) -> GenericApplication NegotiatedResponse) -> GenericApplication Response
withXmlAndJson f =
  withCustomNegotiation $ f $ negotiated negotiationXmlAndJson

negotiationXmlAndJson :: (ToJSON a, ToXML a) => [(BS.ByteString, a -> ByteString)]
negotiationXmlAndJson = 
  [ ("application/json", encode)
  , ("application/xml", toXML)
  ]

class FromXML a where
  fromXML :: ByteString -> Either String a

instance FromXML Pet where
  fromXML bs = $notImplemented

class ToXML a where
  toXML :: a -> ByteString

instance ToXML a => ToXML [a] where
  toXML a = $notImplemented

instance ToXML Pet where
  toXML a = $notImplemented

instance ToXML ErrorResponse where
  toXML a = $notImplemented

-}