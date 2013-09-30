{-# LANGUAGE FunctionalDependencies #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Stored
    ( Exists (..)
    , Fetch (..)
    , Store (..)
    , Update (..)
    , Delete (..)
    ) where

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Proxy


------------------------------------------------------------------------------
-- | A type 'r' can be determined to exist in a monad 'm ' by identifying it
-- with a value of type 'i'.
class Exists r m i | r m -> i where

    -- | Determines if the resource identified by the given value exists.  The
    -- proxy is necessary to get the right instance, and can be ignored in the
    -- implementation of this method.
    exists :: Resource r -> i -> m Bool


------------------------------------------------------------------------------
-- | A type 'r' can be fetched from a monad 'm' by identifying it with a value
-- of type 'i'.
class Exists r m i => Fetch r m i | r m -> i where

    -- | Fetch a resource mathcing the given identifier.  Should return
    -- 'Nothing' if the resource cannot be found.
    fetch  :: i -> m (Maybe r)


------------------------------------------------------------------------------
-- | A type 'r' can be stored using a monad 'm'.
class Store r m where

    -- | Store a given resource.
    store :: r -> m ()


------------------------------------------------------------------------------
-- | A type of 'r' can be updated using a monad 'm', identified by values of
-- type 'i' and diffed by values of type 'd'.
class Exists r m i => Update r m i d | r m -> i d where

    -- | Update a resource from its identifier and a description of what parts
    -- of the value have changed.  The proxy is necessary to get the right
    -- instance, and can be ignored in the implementation.
    update :: Resource r -> i -> d -> m ()


------------------------------------------------------------------------------
-- | A type 'r' can be deleted using a monad 'm', identified by values of type
-- 'i'.
class Exists r m i => Delete r m i | r m -> i where

    -- | Delete a resource from its identifier.  The proxy is necessary to
    -- get the right instance, and can be ignored in the implementation.
    delete :: Resource r -> i -> m ()

