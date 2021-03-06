{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
    ID,
    QUERY,
    Resolver,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    lift,
    render,
  )
import Server.Schema
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

resolveDeity :: DeityArguments -> ComposedResolver o e IO Maybe Deity
resolveDeity DeityArguments {id = "morpheus"} =
  pure $ Just $
    Deity
      { name = pure "Morpheus",
        power = pure [Shapeshifting]
      }
resolveDeity _ = pure Nothing

resolveQuery :: Query (Resolver QUERY e IO)
resolveQuery =
  Query
    { deity = resolveDeity,
      characters = pure []
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
