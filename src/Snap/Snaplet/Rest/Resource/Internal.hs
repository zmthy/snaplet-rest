{-# LANGUAGE RankNTypes #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Internal
    (
    -- * Standard aliases
      Resource
    , ConstantResource
    , StoredResource

    -- * Datatype
    , SplitResource (..)

    -- * Void
    , Void
    ) where

------------------------------------------------------------------------------
import Data.Void (Void)


------------------------------------------------------------------------------
-- | A resource which may have any functionality it wishes.
type Resource res m id diff = SplitResource res res m id diff


------------------------------------------------------------------------------
-- | A resource which may only be fetched.  It cannot be stored.
type ConstantResource res m id = SplitResource res Void m id Void


------------------------------------------------------------------------------
-- | A resource which may be fetched, stored, and deleted.  It cannot be
-- updated.
type StoredResource res m id = SplitResource res res m id Void


------------------------------------------------------------------------------
-- | A resource whose types for retrieving and storing may be distinct.  These
-- types are expected to either be the same or 'Void'.  This type is exposed
-- to allow for other type combinations than the ones supplied above.
data SplitResource rep par m id diff = Resource
    { exists :: Maybe (id -> m Bool)
    , fetch  :: Maybe (id -> m (Maybe rep))
    , store  :: Maybe (par -> m ())
    , update :: Maybe (id -> diff -> m ())
    , delete :: Maybe (id -> m ())
    }

