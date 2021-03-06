{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.API
  ( app,
    httpEndpoint,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus
  ( App,
    deriveApp,
    runApp,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( ComposedResolver,
    QUERY,
    Resolver,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    lift,
    render,
    ID
  )
import Data.Morpheus.App
  ( MapAPI,
  )
import Data.Text (Text)
import Server.Schema
  ( Deity (..),
    DeityArguments (..),
    Query (..),
  )
import Server.Utils (isSchema)
import Web.Scotty
  ( RoutePattern,
    ScottyM,
    body,
    get,
    post,
    raw,
  )
import Prelude hiding (id)

-- RESOLVERS
resolveDeity :: DeityArguments -> ResolverQ e IO Deity
resolveDeity DeityArguments {id}  = pure Deity{ }

resolveQuery :: Query (Resolver QUERY e IO)
resolveQuery =
  Query
    { deity = resolveDeity
    }

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = resolveQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () IO
app = deriveApp rootResolver

httpEndpoint ::
  RoutePattern ->
  App () IO ->
  ScottyM ()
httpEndpoint route app' = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . runApp app' =<< body)
