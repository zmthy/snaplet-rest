{-# LANGUAGE FunctionalDependencies #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Diff
    ( Diff (..)
    ) where


------------------------------------------------------------------------------
class Diff r d | r -> d where
    toDiff :: r -> d

