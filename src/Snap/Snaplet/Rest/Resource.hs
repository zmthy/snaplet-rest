------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource
    (
    -- * Types
      Resource.Resource

    -- * Builder
    , ResourceBuilder
    , buildResource
    , BuildSetter
    , fetch
    , store
    , update
    , delete
    , putAction
    ) where

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Builder

