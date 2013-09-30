{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource
    (
    -- * Serving resources
      serveResource
    , serveResourceWith
    , fetchResource
    , fetchResourceWith
    , storeResource
    , storeResourceWith
    , updateResource
    , updateResourceWith
    , deleteResource
    , deleteResourceWith

    -- * Media
    , FromMedia (..)
    , ToMedia (..)

    -- * Stored
    , Exists (..)
    , Fetch (..)
    , Store (..)
    , Diff (..)

    -- * Config
    , HasResourceConfig (..)
    , ResourceConfig (..)
    , defaultConfig

    -- * Proxy
    , Resource (..)
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config
import Snap.Snaplet.Resource.Diff
import Snap.Snaplet.Resource.Failure
import Snap.Snaplet.Resource.Media
import Snap.Snaplet.Resource.Options
import Snap.Snaplet.Resource.Path
import Snap.Snaplet.Resource.Proxy
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serveResource
    :: (HasResourceConfig m, MonadSnap m, FromMedia r, ToMedia r, FromPath i
    , FromMedia d, Diff r d, Fetch r m i, Store r m , Update r m i d
    , Delete r m i)
    => Resource r -> m ()
serveResource r = fetchResource r
    <|> storeResource r
    <|> updateResource r
    <|> deleteResource r
    <|> checkResource r
    <|> fetchOptions r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: (MonadSnap m, FromMedia r, ToMedia r, FromPath i, FromMedia d
    , Diff r d, Fetch r m i, Store r m, Update r m i d, Delete r m i)
    => Resource r -> ResourceConfig m -> m ()
serveResourceWith r cfg = fetchResourceWith r cfg
    <|> storeResourceWith r cfg
    <|> updateResourceWith r cfg
    <|> deleteResourceWith r cfg
    <|> checkResourceWith r cfg
    <|> fetchOptionsWith r cfg


------------------------------------------------------------------------------
-- | Fetch and serve a resource using the remaining path information, using
-- the configuration in the monad.
fetchResource
    :: (HasResourceConfig m, MonadSnap m, ToMedia r, FromPath i, Fetch r m i)
    => Resource r -> m ()
fetchResource r = method GET (resourceConfig >>= fetchResourceWith r)
    <|> checkResource r <|> fetchOptions r


------------------------------------------------------------------------------
-- | Fetch and serve a resource using the remaining path information, using
-- the given configuration.
fetchResourceWith
    :: (MonadSnap m, ToMedia r, FromPath i, Fetch r m i)
    => Resource r -> ResourceConfig m -> m ()
fetchResourceWith r cfg = method GET $ fetchResourceWith' r cfg
    <|> checkResourceWith r cfg <|> fetchOptionsWith r cfg


------------------------------------------------------------------------------
-- | Unrouted form of 'fetchResourceWith'.
fetchResourceWith'
    :: forall m r i. (MonadSnap m, ToMedia r, FromPath i, Fetch r m i)
    => Resource r -> ResourceConfig m -> m ()
fetchResourceWith' _ cfg = getRequest >>= maybe (pathFailure cfg)
    (fetch >=> maybe (lookupFailure cfg) serve) . fromPath . rqPathInfo
  where serve = serveMediaWith cfg :: r -> m ()


------------------------------------------------------------------------------
-- | Store a new resource from the request body, using the configuration in
-- the monad.
storeResource
    :: (HasResourceConfig m, MonadSnap m, FromMedia r, Store r m)
    => Resource r -> m ()
storeResource r = method POST $ resourceConfig >>= storeResourceWith' r


------------------------------------------------------------------------------
-- | Store a new resource from the request body, using the given
-- configuration.
storeResourceWith
    :: (MonadSnap m, FromMedia r, Store r m)
    => Resource r -> ResourceConfig m -> m ()
storeResourceWith r cfg = method POST $ storeResourceWith' r cfg


------------------------------------------------------------------------------
-- | Unrouted form of 'storeResourceWith'.
storeResourceWith'
    :: forall m r. (MonadSnap m, FromMedia r, Store r m)
    => Resource r -> ResourceConfig m -> m ()
storeResourceWith' _ cfg = ifTop (receive >>= store) <|> methodFailure cfg
  where receive = receiveMediaWith cfg :: m r



------------------------------------------------------------------------------
-- | Update a resource from the request body, using the configuration in the
-- monad.
updateResource
    :: (HasResourceConfig m, MonadSnap m, FromMedia r, FromPath i, FromMedia d
    , Diff r d, Update r m i d)
    => Resource r -> m ()
updateResource r = methods [PUT, PATCH]
    (resourceConfig >>= updateResourceWith r)
    <|> checkResource r <|> fetchOptions r


------------------------------------------------------------------------------
-- | Update a resource from the request body, using the given configuration.
updateResourceWith
    :: forall m r i d. (MonadSnap m, FromMedia r, FromPath i, Diff r d
    , FromMedia d, Update r m i d)
    => Resource r -> ResourceConfig m -> m ()
updateResourceWith r cfg =
    (method PUT (toDiff <$> (receiveMediaWith cfg :: m r))
        <|> method PATCH (receiveMediaWith cfg :: m d)
            >>= updateResourceWith' r cfg)
    <|> checkResourceWith r cfg <|> fetchOptionsWith r cfg


------------------------------------------------------------------------------
-- | Unrouted form of 'updateResourceWith'.
updateResourceWith'
    :: (MonadSnap m, FromPath i, Diff r d, Update r m i d)
    => Resource r -> ResourceConfig m -> d -> m ()
updateResourceWith' r cfg diff = getRequest >>=
    maybe (pathFailure cfg) (flip (update r) diff) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Delete a resource, using the configuration in the monad.
deleteResource
    :: (HasResourceConfig m, MonadSnap m, FromPath i, Delete r m i)
    => Resource r -> m ()
deleteResource r = method DELETE (resourceConfig >>= deleteResourceWith' r)
    <|> checkResource r <|> fetchOptions r


------------------------------------------------------------------------------
-- | Delete a resource, using the given configuration.
deleteResourceWith
    :: (MonadSnap m, FromPath i, Delete r m i)
    => Resource r -> ResourceConfig m -> m ()
deleteResourceWith r cfg = method DELETE (deleteResourceWith' r cfg)
    <|> checkResourceWith r cfg <|> fetchOptionsWith r cfg


------------------------------------------------------------------------------
-- | Unrouted form of 'deleteResourceWith'.
deleteResourceWith'
    :: (MonadSnap m, FromPath i, Delete r m i)
    => Resource r -> ResourceConfig m -> m ()
deleteResourceWith' r cfg = getRequest >>=
    maybe (pathFailure cfg) (delete r) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Similar to 'fetchResource', but only checks if the resource exists,
-- serving an empty body.
checkResource
    :: (HasResourceConfig m, MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> m ()
checkResource r = method HEAD $ resourceConfig >>= checkResourceWith' r


------------------------------------------------------------------------------
-- | Similar to 'fetchResource', but only checks if the resource exists,
-- serving an empty body.
checkResourceWith
    :: (MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> ResourceConfig m -> m ()
checkResourceWith r cfg = method HEAD $ checkResourceWith' r cfg


------------------------------------------------------------------------------
-- | Unrouted form of 'checkResourceWith'.
checkResourceWith'
    :: (MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> ResourceConfig m -> m ()
checkResourceWith' r cfg = getRequest >>= maybe (pathFailure cfg)
    (exists r >=> flip when (lookupFailure cfg) . not) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptions
    :: (HasResourceConfig m, MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> m ()
fetchOptions r = method OPTIONS $ resourceConfig >>= fetchOptionsWith' r


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptionsWith
    :: (MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> ResourceConfig m -> m ()
fetchOptionsWith r cfg = method OPTIONS $ fetchOptionsWith' r cfg


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptionsWith'
    :: (MonadSnap m, FromPath i, Exists r m i)
    => Resource r -> ResourceConfig m -> m ()
fetchOptionsWith' r cfg = ifTop (serveMediaWith cfg CollectionOptions) <|> do
    getRequest >>= maybe (pathFailure cfg) return . fromPath . rqPathInfo >>=
        exists r >>= flip when (lookupFailure cfg) . not
    serveMediaWith cfg ResourceOptions

