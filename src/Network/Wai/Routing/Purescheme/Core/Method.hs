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
{-# LANGUAGE TemplateHaskell #-}
module Network.Wai.Routing.Purescheme.Core.Method
  ( method
  )
where

import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Internal

import qualified Data.Text.Encoding as T
import Network.HTTP.Types (StdMethod, renderStdMethod, methodNotAllowed405, statusMessage)
import Network.Wai (requestMethod)

-- | Match with standard http method
method :: StdMethod -> GenericApplication e -> GenericApplication e
method m f req =
  if requestMethod req == renderStdMethod m
    then f req
    else reject methodNotAllowedRejection

methodNotAllowedRejection :: Rejection
methodNotAllowedRejection =
  Rejection
    { status = methodNotAllowed405
    , message = T.decodeUtf8 $ statusMessage methodNotAllowed405
    , priority = 100
    , headers = []
    }
