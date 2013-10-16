------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Internal
    (
    -- * Types
      Resource (..)
    , PutAction (..)

    -- * Void
    , Void
    ) where

------------------------------------------------------------------------------
import Data.Void (Void)
import Snap.Core (Params)


------------------------------------------------------------------------------
-- | A resource whose types for retrieving and storing may be distinct.  These
-- types are expected to either be the same or 'Void'.  This type is exposed
-- to allow for other type combinations than the ones supplied above.
data Resource rep par m id diff = Resource
    { fetch      :: Maybe (id -> m [rep])
    , store      :: Maybe (par -> m ())
    , update     :: Maybe (id -> diff -> m Bool)
    , delete     :: Maybe (id -> m Bool)
    , fromParams :: Maybe (Params -> Maybe id)
    , putAction  :: PutAction
    }


------------------------------------------------------------------------------
-- | Indicates which action that a PUT request should take for a resource.
data PutAction
    = TryUpdate   -- ^ Attempt to update, store if that fails
    | JustStore   -- ^ Always store
    | JustUpdate  -- ^ Always update
    deriving (Eq, Show)

