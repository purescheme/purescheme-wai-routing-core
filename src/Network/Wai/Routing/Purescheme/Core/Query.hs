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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Routing.Purescheme.Core.Query
  ( singleParameter
  , maybeSingleParameter
  )
where
  
import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Internal

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.String.Interpolate.IsString (i)
import Network.HTTP.Types (badRequest400, statusMessage)
import Network.Wai (queryString)

-- | Match single parameter in the query string, fails when the parameter is not found or the
-- query string contains multiple values for the parameter
singleParameter :: FromUri a => ByteString -> (a -> GenericApplication b) -> GenericApplication b
singleParameter name f req =
  case filter (\(k, _) -> k == name) $ queryString req of
    [(_, Just value)] -> f (fromByteString value) req
    [] -> reject $ invalidParameterRejection [i|Required query parameter not found: #{name}|]
    _ -> reject $ invalidParameterRejection 
      [i|Found more than one parameter in query string, required only one: #{name}|]

-- | Match single parameter in the query string, if multiple values for the same parameter found
-- then fails
maybeSingleParameter :: 
  FromUri a 
  => ByteString 
  -> (Maybe a -> GenericApplication r) 
  -> GenericApplication r
maybeSingleParameter name f req =
  case filter (\(k, _) -> k == name) $ queryString req of
    [(_, Just value)] -> f (Just $ fromByteString value) req
    [] -> f Nothing req
    _ -> 
      reject $ invalidParameterRejection 
        [i|Found more than one parameter in query string, required none or only one: #{name}|]

invalidParameterRejection :: T.Text -> Rejection
invalidParameterRejection errorMessage =
  Rejection
    { status = badRequest400
    , message = [i|#{statusMessage badRequest400}: #{errorMessage}|]
    , priority = 200
    , headers = []
    }

