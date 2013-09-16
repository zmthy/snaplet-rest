{-# LANGUAGE FunctionalDependencies #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Stored
    ( Stored (..)
    ) where

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Proxy


------------------------------------------------------------------------------
-- | A type 'r' can be stored using a monad 'm', can be identified by
-- values of a type 'i', and diffed by a type 'd'.
class Stored m r i d | m r -> i d where

    -- | Attempt to retrieve a resource from an identifier.  Should return
    -- 'Nothing' if the resource cannot be found.
    retrieve :: i -> m (Maybe r)

    -- | Store a given resource.
    store    :: r -> m ()

    -- | Delete a resource from its identifier.  The proxy is necessary to
    -- get the right instance, and can be ignored in the implementation.
    delete   :: Resource r -> i -> m ()

    -- | Update a resource from its identifier and a description of what parts
    -- of the value have changed.  The proxy is necessary to get the right
    -- instance, and can be ignored in the implementation.
    update   :: Resource r -> i -> d -> m ()

