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
module Network.Wai.Routing.Purescheme.Core.Header
  ( headerValue
  , headerValue'
  )
where
  
import Network.Wai.Routing.Purescheme.Core.Basic

import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (requestHeaders)

-- | Extract the value of the first HTTP request header with a given name (ByteString)
headerValue :: ByteString -> (Maybe ByteString -> GenericApplication b) -> GenericApplication b
headerValue name = headerValue' (CI.mk name)

-- | Extract the value of the fist HTTP request header with a given header name
headerValue' :: HeaderName -> (Maybe ByteString -> GenericApplication b) -> GenericApplication b
headerValue' name f req = 
  let 
    maybeValue = lookup name (requestHeaders req)
  in
    f maybeValue req