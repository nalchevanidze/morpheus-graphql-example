{-# LANGUAGE OverloadedStrings #-}

module Server.Scotty
  ( scottyServer,
  )
where

import Server.API (app, httpEndpoint)
import Server.Utils (startServer)

scottyServer :: IO ()
scottyServer = startServer (httpEndpoint "/" app)
