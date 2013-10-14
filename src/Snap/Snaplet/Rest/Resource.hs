------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource
    (
    -- * Types
      Resource.Resource

    -- * Builder
    , ResourceBuilder
    , buildResource
    , setFetch
    , setStore
    , setUpdate
    , setDelete
    , setPutAction
    ) where

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Builder

