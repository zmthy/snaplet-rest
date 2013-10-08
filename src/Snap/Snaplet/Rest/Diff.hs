{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Diff
    ( Diff (..)
    ) where

------------------------------------------------------------------------------
import Data.Void (Void)


------------------------------------------------------------------------------
-- | A value of type 'diff' represents a partial form of a value of type
-- 'res', indicating changes to that type.
class Diff r d where

    -- | Given a full value, produce a diff value.  Indicates that everything
    -- should be changed.
    toDiff :: r -> d

instance Diff r Void where
    toDiff _ = error "Cannot produce the diff of Void"

