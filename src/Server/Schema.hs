{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Schema
  ( Deity (..),
    Query (..),
    DeityArguments (..),
    Character(..), 
    Power(..)
  )
where

import Data.Morpheus.Types
  ( GQLType (..),ID
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Power
  = Shapeshifting
  | Thunderbolt
  deriving
    ( Generic,
      GQLType
    )

data Deity m = Deity
  { name :: m Text,
    power :: m [Power]
  }
  deriving
    ( Generic,
      GQLType
    )

data Character m 
  = CharacterDeity (Deity m)
  | Titan { name :: m Text}
  | UnknownCreature
  deriving
    ( Generic,
      GQLType
    )

newtype DeityArguments = DeityArguments {id :: ID}
  deriving
    ( Generic,
      GQLType
    )

data Query m = Query
  { deity :: DeityArguments -> m (Maybe (Deity m)),
    characters :: m [Character m]
  }
  deriving
    ( Generic,
      GQLType
    )
