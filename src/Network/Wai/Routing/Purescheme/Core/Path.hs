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
module Network.Wai.Routing.Purescheme.Core.Path
  ( path
  , pathSegment
  , pathVar
  , pathEnd
  )
where

import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Internal

import Data.Text (Text, intercalate)
import Network.Wai (pathInfo)

-- | Match the remaining path
path :: Text -> GenericApplication r -> GenericApplication r
path p app req =
  if p == intercalate "/" (pathInfo req)
    then app $ req {pathInfo = []}
    else reject notFoundDefaultRejection

-- | Match the next path segment and remove from the request
pathSegment :: Text -> GenericApplication r -> GenericApplication r
pathSegment expectedSegment app req =
  case pathInfo req of
    (p:rest) | p == expectedSegment -> app $ req {pathInfo = rest}
    _ -> reject notFoundDefaultRejection

-- | Use the next path segment as a variable and remove from the request
pathVar :: FromUri a => (a -> GenericApplication r) -> GenericApplication r
pathVar f req = 
  case pathInfo req of
    [] -> reject notFoundDefaultRejection
    [""] -> reject notFoundDefaultRejection
    (p:rest) -> f (fromText p) (req{pathInfo = rest})

-- | Match if all the path has been consumed or the remaining is a trailing slash
pathEnd :: GenericApplication r -> GenericApplication r
pathEnd f req = 
  case pathInfo req of
    [] -> f req
    [""] -> f req{pathInfo = []}
    _ -> reject notFoundDefaultRejection

