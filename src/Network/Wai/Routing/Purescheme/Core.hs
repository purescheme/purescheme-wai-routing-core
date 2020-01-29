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
-- |
-- Module     : Network.Wai.Routing.Purescheme.Core
-- Copyright  : (c) Fernando Rincon Martin 2020
-- License    : Apache-2.0 (see the file LICENSE)
-- 
-- Maintainer : Fernando Rincon Martin <f.rincon@protonmail.com>
-- Stability: alpha
-- 
-- This module provides simple routing functions that works on top of "Network.Wai" 
-- applications.
-- 
-- The basic idea is provides functions that modifies an @"Application" in order to match
-- certain rules. It is inspired on akka http server DSL.
--
-- A simple example of a Json rest api:
-- 
-- >   restApi :: Application
-- >   restApi = path "hello" 
-- >               $ method GET 
-- >               $ complete 
-- >               $ responseLBS status200 [(hContentType, "text/html")] "<h1>Hellow World!</h1>"
-- 
-- As the result is a Wai @'Application' we can run it directly with warp server:
--
-- >   main :: IO ()
-- >   main = run 8080 restApi
--
-- The api is in alpha state, so that the api can change in any new release. It is very welcome 
-- suggestions and comments.

module Network.Wai.Routing.Purescheme.Core ( 
  -- * Basic functionality
  -- ** Basic Types and Classes
    GenericApplication
  , Rejection(..)
  , FromUri(..)
  , HasResponseHeaders(..)
  -- ** Basic combinators
  , alternatives
  -- ** Exception handling
  , handleException
  , withDefaultExceptionHandler
  -- ** Complete or reject requests
  , complete
  , completeIO
  -- ** Response Manipulatins
  , mapResponse
  -- ** Requests functions
  , withRequest
  -- * Uri path functions
  , path
  , pathSegment
  , pathVar
  , pathEnd
  -- * Query string functions
  , singleParameter
  , maybeSingleParameter
  -- * Http method 
  , method
  -- * Entity based
  -- ** Entity Based types
  , EntityResponse
  , entity
  , NegotiatedResponse
  -- ** Basic entity functions
  , mapEntity
  , withCustomNegotiation
  , withCustomNegotiation'
  , negotiated
  , requestEntity
  , ok
  , created
  , notFound
  , badRequest
  , entityResponse
  -- ** Json based entity functions
  , withContentNegotiationJson
  , entityJson
)
where

import Network.Wai.Routing.Purescheme.Core.Basic
import Network.Wai.Routing.Purescheme.Core.Path
import Network.Wai.Routing.Purescheme.Core.Query
import Network.Wai.Routing.Purescheme.Core.Method
import Network.Wai.Routing.Purescheme.Core.Entity
import Network.Wai.Routing.Purescheme.Core.Entity.Json




