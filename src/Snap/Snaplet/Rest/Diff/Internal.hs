{-# LANGUAGE FlexibleInstances #-}

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

    -- | Internal method which disables PATCH for the two instances below.
    -- All other instances will keep PATCH enabled.
    patchDisabled :: Proxy res diff -> Bool
    patchDisabled _ = True

-- This instance allows 'Void' to be used as the diff type, indicating no
-- update method is available.  This disables PUT and PATCH.
instance Diff res Void where
    toDiff _ = error "Cannot produce the diff of Void"
    patchDisabled _ = False

-- This instance allows a resource to be its own diff type, indicating no
-- partial update method is available.  This disables PATCH.
instance Diff res res where
    toDiff = id
    patchDisabled _ = False


------------------------------------------------------------------------------
data Proxy r d = Proxy

