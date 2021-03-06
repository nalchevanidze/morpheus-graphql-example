{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Server.App (app) where

import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Types
  ( ComposedResolver,
    QUERY,
    Resolver,
    RootResolver (..),
    Undefined (..),
  )
import Server.Schema
import Prelude hiding (id)

resolveDeity :: DeityArguments -> ComposedResolver o e IO Maybe Deity
resolveDeity DeityArguments {id = "morpheus"} =
  pure $ Just $
    Deity
      { name = pure "Morpheus",
        power = pure [Shapeshifting]
      }
resolveDeity _ = pure Nothing

resolveCharacters :: ComposedResolver o e IO [] Character
resolveCharacters =
  pure
    [ UnknownCreature,
      CharacterDeity
        ( Deity
            { name = pure "Morpheus",
              power = pure [Shapeshifting]
            }
        ),
      Titan {name = pure "Prometheus"}
    ]

resolveQuery :: Query (Resolver QUERY e IO)
resolveQuery =
  Query
    { deity = resolveDeity,
      characters = resolveCharacters
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
