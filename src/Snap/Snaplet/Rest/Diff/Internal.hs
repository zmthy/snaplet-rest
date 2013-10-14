{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Diff.Internal
    ( Diff (..)
    , Proxy (..)
    ) where

------------------------------------------------------------------------------
import Data.Void (Void)


------------------------------------------------------------------------------
-- | A value of type 'diff' represents a partial form of a value of type
-- 'res', indicating changes to that type.
class Diff res diff where

    -- | Given a full value, produce a diff value.  Indicates that everything
    -- should be changed.
    toDiff :: res -> diff

    -- | Internal method which disables PATCH for the three instances below.
    -- All other instances will keep PATCH enabled.
    patchDisabled :: Proxy res diff -> Bool
    patchDisabled _ = True

-- This instance allows 'Void' to be used as the diff type, indicating no
-- update method is available.  This disables PUT and PATCH.
instance Diff res Void where
    toDiff _ = error "Cannot produce a void diff"
    patchDisabled _ = False

-- This instance allows 'Void' to be used as the resource type, indicating no
-- store method is available.  This disables PUT (and POST), but not PATCH.
instance Diff Void res where
    toDiff _ = error "Cannot produce the diff of void"

-- This instance allows a resource to be its own diff type, indicating no
-- partial update method is available.  This disables PATCH, but not PUT.
instance Diff res res where
    toDiff = id
    patchDisabled _ = False

-- This instance accounts for the overlapping instance between the three
-- previous instances, which indicates neither store or update is available.
-- This disables POST, PUT, and PATCH.
instance Diff Void Void where
    toDiff _ = error "Cannot produce a void diff"
    patchDisabled _ = False


------------------------------------------------------------------------------
data Proxy r d = Proxy

