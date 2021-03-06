{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Server.App (app)
import Server.Utils (httpEndpoint, startServer)

main :: IO ()
main = startServer (httpEndpoint "/" app)
