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
    , setFromParams
    , setPutAction
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Data.Maybe
import Data.Void           (Void)
import Snap.Core           (Params)

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.FromRequest.Internal
import Snap.Snaplet.Rest.Resource.Internal
    (Resource (Resource), PutAction (..))
import Snap.Snaplet.Rest.Proxy


------------------------------------------------------------------------------
data ResourceBuilder rep par m id diff = ResourceBuilder
    { fetch      :: Maybe (id -> m [rep])
    , store      :: Maybe (par -> m ())
    , update     :: Maybe (id -> diff -> m Bool)
    , delete     :: Maybe (id -> m Bool)
    , fromParams :: Maybe (Params -> Maybe id)
    , putAction  :: Maybe PutAction
    }


------------------------------------------------------------------------------
buildResource
    :: (Functor m, FromRequest id)
    => (ResourceBuilder Void Void m Void Void
        -> ResourceBuilder rep par m id diff)
    -> Resource rep par m id diff
buildResource f = Resource
    { Resource.fetch      = fetch rb
    , Resource.store      = store rb
    , Resource.update     = update rb
    , Resource.delete     = delete rb
    , Resource.fromParams = fromParams rb <|> defaultFromParams
    , Resource.putAction  = fromMaybe putDefault $ putAction rb
    }
  where
    rb = f $ ResourceBuilder Nothing Nothing Nothing Nothing Nothing Nothing
    hasStore  = isJust $ store rb
    hasUpdate = isJust $ update rb
    putDefault
        | hasStore && not hasUpdate = JustStore
        | hasUpdate && not hasStore = JustUpdate
        | otherwise                 = TryUpdate


------------------------------------------------------------------------------
class UnVoid a b x y where
    voidCast :: Proxy (a, b) -> Maybe x -> Maybe y

instance UnVoid Void b x y where
    voidCast _ _ = Nothing

instance UnVoid a a x x where
    voidCast _ = id


------------------------------------------------------------------------------
setFetch
    :: (UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => (id' -> m [rep])
    -> ResourceBuilder Void par m id diff
    -> ResourceBuilder rep par m id' diff
setFetch = setFetch' Proxy

setFetch'
    :: (UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => Proxy (id, id')
    -> (id' -> m [rep])
    -> ResourceBuilder Void par m id diff
    -> ResourceBuilder rep par m id' diff
setFetch' p a rb = rb
    { fetch      = Just a
    , update     = voidCast p $ update rb
    , delete     = voidCast p $ delete rb
    , fromParams = voidCast p $ fromParams rb
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
    , UnVoid id id' (id -> m Bool) (id' -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => (id' -> diff -> m Bool)
    -> ResourceBuilder rep par m id Void
    -> ResourceBuilder rep par m id' diff
setUpdate = setUpdate' Proxy

setUpdate'
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> m Bool) (id' -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => Proxy (id, id')
    -> (id' -> diff -> m Bool)
    -> ResourceBuilder rep par m id Void
    -> ResourceBuilder rep par m id' diff
setUpdate' p a rb = rb
    { fetch      = voidCast p $ fetch rb
    , update     = Just a
    , delete     = voidCast p $ delete rb
    , fromParams = voidCast p $ fromParams rb
    }


------------------------------------------------------------------------------
setDelete
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => (id' -> m Bool)
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setDelete = setDelete' Proxy

setDelete'
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (Params -> Maybe id) (Params -> Maybe id'))
    => Proxy (id, id')
    -> (id' -> m Bool)
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setDelete' p a rb = rb
    { fetch      = voidCast p $ fetch rb
    , update     = voidCast p $ update rb
    , delete     = Just a
    , fromParams = voidCast p $ fromParams rb
    }


------------------------------------------------------------------------------
setFromParams
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => (Params -> Maybe id')
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setFromParams = setFromParams' Proxy

setFromParams'
    :: (UnVoid id id' (id -> m [rep]) (id' -> m [rep])
    , UnVoid id id' (id -> diff -> m Bool) (id' -> diff -> m Bool)
    , UnVoid id id' (id -> m Bool) (id' -> m Bool))
    => Proxy (id, id')
    -> (Params -> Maybe id')
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id' diff
setFromParams' p a rb = rb
    { fetch      = voidCast p $ fetch rb
    , update     = voidCast p $ update rb
    , delete     = voidCast p $ delete rb
    , fromParams = Just a
    }


------------------------------------------------------------------------------
setPutAction
    :: PutAction
    -> ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id diff
setPutAction a rb = rb { putAction = Just a }

