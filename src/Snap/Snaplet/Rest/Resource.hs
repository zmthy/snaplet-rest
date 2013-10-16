------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource
    (
    -- * Type
      Resource.Resource

    -- * Builder
    , ResourceBuilder
    , buildResource
    , setFetch
    , setStore
    , setUpdate
    , setDelete
    , setFromParams
    , setPutAction
    ) where

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Builder

