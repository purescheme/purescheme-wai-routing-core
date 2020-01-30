# purescheme-wai-routing-core
[![Build Status](https://travis-ci.org/purescheme/purescheme-wai-routing-core.svg?branch=master)](https://travis-ci.org/purescheme/purescheme-wai-routing-core)
![Hackage](https://img.shields.io/hackage/v/purescheme-wai-routing-core)

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
main = run 8080 api

api :: Application
api = path "hello"
            $ method GET
            $ complete
            $ responseLBS status200 [(hContentType, "text/html")] "<h1>Hellow World!</h1>"

```

## Overview

This module provides simple routing functions that works on top of "Network.Wai"
applications.
 
The basic idea is provides functions that modifies an @"Application" in order to match
certain rules. It is inspired on akka http server Routing DSL.

## User Guide

The API is divided in several modules by its functionality:
- `Network.Wai.Routing.Purescheme.Core.Basic` provides the basic types and functionality including how to complete a request and exception handling.
- `Network.Wai.Routing.Purescheme.Core.Path` routing functions that matches with the Path of the URI.
- `Network.Wai.Routing.Purescheme.Core.Query` routing functions to extract parameters from the query part of the URI.
- `Network.Wai.Routing.Purescheme.Core.Method` routing functions based on http method.
- `Network.Wai.Routing.Purescheme.Core.Entity` routing and manipulation API related with Entity in the request and the response. Useful for content negotiation and serialization of entities.
- `Network.Wai.Routing.Purescheme.Core.Entity.Json` concrete functions to operate on JSON encoding entities based on Aeson library.

A complete example can be found in the [app](https://github.com/purescheme/purescheme-wai-routing-core/tree/master/app) folder.

## Why
There are many routing frameworks for Haskell but they usually are:
- They come with their own Monads or exoteric strategics to build routes.
- Some of them are not using the type system properly

So, as Haskell is totally functional, and, based on the simplicity of an Wai `Application` (which is basically
a function that converts requests to responses, why not create functions on top of Wai `Application` that 
provides a functional way to implement RESTful APIs?

## Status
Currently the API is totally functionalm but the status is considered Alpha. That means, the whole API can change
in further release until we reach the beta status.

## Feedback are welcome!
Please, if you feel that some functionallity is missing or something can be improve, post a issue!
