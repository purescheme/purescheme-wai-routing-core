# purescheme-wai-routing-core

*The goal of purescheme is make a simple framework for building fast microservices in haskell.*

This module provides simple routing functions for create rest APIs on top of a WAI server.

## Getting Started

Minimal Example to run a helloworld using Warp server:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Routing.Purescheme.Core

import Network.HTTP.Types (status200, StdMethod(..), hContentType)
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseLBS, Application)

main :: IO ()
main = run 8080 $ api

api :: Application
api = path "hello"
            $ method GET
            $ complete
            $ responseLBS status200 [(hContentType, "text/html")] "<h1>Hellow World!</h1>"

```
