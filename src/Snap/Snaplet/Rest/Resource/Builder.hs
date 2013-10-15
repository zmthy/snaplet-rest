{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Builder
    (
    -- * Builder
      ResourceBuilder
    , buildResource

    -- * Setters
    , setFetch
    , setStore
    , setUpdate
    , setDelete
    , setPutAction
    ) where

------------------------------------------------------------------------------
import Data.Maybe
import Data.Void  (Void)

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal
    (Resource (Resource), PutAction (..))


------------------------------------------------------------------------------
data ResourceBuilder rep par m id diff = ResourceBuilder
    { fetch     :: Maybe (id -> m [rep])
    , store     :: Maybe (par -> m ())
    , update    :: Maybe (id -> diff -> m Bool)
    , delete    :: Maybe (id -> m Bool)
    , putAction :: Maybe PutAction
    }


------------------------------------------------------------------------------
buildResource
    :: Functor m => (ResourceBuilder Void Void m Void Void
    -> ResourceBuilder rep par m id diff)
    -> Resource rep par m id diff
buildResource f = Resource
    { Resource.fetch     = fetch rb
    , Resource.store     = store rb
    , Resource.update    = update rb
    , Resource.delete    = delete rb
    , Resource.putAction = fromMaybe putDefault $ putAction rb
    }
  where
    rb = f $ ResourceBuilder Nothing Nothing Nothing Nothing Nothing
    putDefault
        | isJust $ store rb  = JustStore
        | isJust $ update rb = JustUpdate
        | otherwise          = TryUpdate


------------------------------------------------------------------------------
class UnVoid a b x y where
    voidCast :: Proxy a b -> Maybe x -> Maybe y

instance UnVoid Void b x y where
    voidCast _ _ = Nothing

instance UnVoid a a x x where
    voidCast _ = id

data Proxy a b = Proxy


------------------------------------------------------------------------------
setFetch
    :: (UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => (id' -> m [rep])
    -> ResourceBuilder Void par m id diff
    -> ResourceBuilder rep par m id' diff
setFetch = setFetch' Proxy

setFetch'
    :: (UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => Proxy id id'
    -> (id' -> m [rep])
    -> ResourceBuilder Void par m id diff
    -> ResourceBuilder rep par m id' diff
setFetch' p a rb = rb
    { fetch  = Just a
    , update = voidCast p $ update rb
    , delete = voidCast p $ delete rb
    }


------------------------------------------------------------------------------
setStore
    :: (par -> m ())
    -> ResourceBuilder rep Void m id diff
    -> ResourceBuilder rep par m id diff
setStore a rb = rb { store = Just a }


------------------------------------------------------------------------------
setUpdate
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => (id' -> diff -> m Bool)
    -> ResourceBuilder rep par m id Void
    -> ResourceBuilder rep par m id' diff
setUpdate = setUpdate' Proxy

setUpdate'
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => Proxy id id'
    -> (id' -> diff -> m Bool)
    -> ResourceBuilder rep par m id Void
    -> ResourceBuilder rep par m id' diff
setUpdate' p a rb = rb
    { fetch  = voidCast p $ fetch rb
    , update = Just a
    , delete = voidCast p $ delete rb
    }


------------------------------------------------------------------------------
setDelete
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool))
    => (id' -> m Bool)
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setDelete = setDelete' Proxy

setDelete'
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool))
    => Proxy id id'
    -> (id' -> m Bool)
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setDelete' p a rb = rb
    { fetch  = voidCast p $ fetch rb
    , update = voidCast p $ update rb
    , delete = Just a
    }


------------------------------------------------------------------------------
setPutAction
    :: PutAction
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id diff
setPutAction a rb = rb { putAction = Just a }

