------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource
    (
    -- * Types
      Resource.Resource
    , Resource.ConstantResource
    , Resource.StoredResource
    , Resource.SplitResource

    -- * Builder
    , ResourceBuilder
    , buildResource
    , exists
    , fetch
    , store
    , update
    , delete
    ) where

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Builder

