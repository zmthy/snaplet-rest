{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest
    (
    -- * Serving resources
      serveResource
    , serveResourceWith

    -- * Media
    , FromMedia (..)
    , ToMedia (..)

    -- * Resource
    , Resource

    -- * Builder
    , Builder.ResourceBuilder
    , Builder.buildResource
    , Builder.setFetch
    , Builder.setStore
    , Builder.setUpdate
    , Builder.setDelete
    , Builder.setPutAction

    -- * Diff
    , Diff (toDiff)

    -- * Config
    , ResourceConfig (..)
    , defaultConfig
    , HasResourceConfig (..)
    , Resources
    , resourceInit
    , resourceInitDefault
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Maybe
import Snap.Core
import Snap.Snaplet        (Handler)

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Builder as Builder

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Config
import Snap.Snaplet.Rest.Diff.Internal
import Snap.Snaplet.Rest.Failure
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Options
import Snap.Snaplet.Rest.Path
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serveResource
    :: (HasResourceConfig b, FromMedia par (Handler b b)
    , ToMedia rep (Handler b b), FromPath id, FromMedia diff (Handler b b)
    , Diff par diff)
    => Resource rep par (Handler b b) id diff -> Handler b b ()
serveResource res = getResourceConfig >>= serveResourceWith res


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: forall rep par m id diff. (MonadSnap m, FromMedia par m, ToMedia rep m
    , FromPath id, FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> m ()
serveResourceWith res cfg =
        serveRoute GET fetchResourceWith (fetch res)
    <|> serveRoute POST postResource (store res)
    <|> serveRoute DELETE deleteResourceWith (delete res)
    <|> servePut (putAction res)
    <|> serveRoute PATCH updateResource (update res)
    <|> serveRoute OPTIONS fetchOptionsWith (Just res)
    <|> serveRoute HEAD fetchResourceWith (fetch res)
  where
    serveRoute mt rt mf = method mt $ maybe methodFailure' (rt cfg) mf
    servePut action = case action of
        TryUpdate  -> flip (serveRoute PUT) (both (store res) (update res)) $
            \cfg' (store', update') -> do
                par <- receiveMediaWith cfg'
                exists <- updateResourceWith' cfg' update' $ toDiff par
                unless exists $ store' par
        JustStore  -> serveRoute PUT storeResourceWith (store res)
        JustUpdate -> flip (serveRoute PUT) (update res) $
            \cfg' update' -> updateResourceWith toDiff' cfg' update'
        Disabled   -> method PUT methodFailure'
    both ma mb = (,) <$> ma <*> mb
    toDiff' = toDiff :: par -> diff
    postResource cfg' store' =
        ifTop (storeResourceWith cfg' store') <|> methodFailure'
    updateResource cfg' update' = do
        guard (not $ patchDisabled (Proxy :: Proxy par diff))
        updateResourceWith id cfg' update'
    methodFailure' = methodFailure res cfg


------------------------------------------------------------------------------
-- | Fetch and serve a resource using the remaining path information.
fetchResourceWith
    :: (MonadSnap m, ToMedia rep m, FromPath id)
    => ResourceConfig m -> (id -> m (Maybe rep)) -> m ()
fetchResourceWith cfg fetch' = parsePath >>= maybe (pathFailure cfg)
    (fetch' >=> maybe (lookupFailure cfg) (serveMediaWith cfg))


------------------------------------------------------------------------------
-- | Store a new resource from the request body.
storeResourceWith
    :: (MonadSnap m, FromMedia par m)
    => ResourceConfig m -> (par -> m ()) -> m ()
storeResourceWith cfg store' = receiveMediaWith cfg >>= store'


------------------------------------------------------------------------------
-- | Update a resource from the request body.  Sends a lookup failure if the
-- update failed.
updateResourceWith
    :: (MonadSnap m, FromMedia par m, FromPath id, FromMedia diff m)
    => (par -> diff) -> ResourceConfig m -> (id -> diff -> m Bool) -> m ()
updateResourceWith toDiff' cfg update' = toDiff' <$> receiveMediaWith cfg
    >>= updateResourceWith' cfg update' >>= flip unless (lookupFailure cfg)


------------------------------------------------------------------------------
-- | Update a resource with the given value.  Returns 'True' if the update was
-- successful.
updateResourceWith'
    :: (MonadSnap m, FromPath id)
    => ResourceConfig m -> (id -> diff -> m Bool) -> diff -> m Bool
updateResourceWith' cfg update' diff = parsePath >>=
    maybe (pathFailure cfg) (flip update' diff)


------------------------------------------------------------------------------
-- | Delete a resource.
deleteResourceWith
    :: (MonadSnap m, FromPath id)
    => ResourceConfig m -> (id -> m Bool) -> m ()
deleteResourceWith cfg delete' = parsePath >>= maybe (pathFailure cfg)
    (delete' >=> flip unless (lookupFailure cfg))


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptionsWith
    :: forall rep par m id diff. (MonadSnap m, FromPath id)
    => ResourceConfig m -> Resource rep par m id diff -> m ()
fetchOptionsWith cfg res = do
    ifNotTop $ isNothing <$> parsePath' >>= flip when (pathFailure cfg)
    setAllow $ optionsFor res
    modifyResponse $ setContentLength 0
  where
    ifNotTop = (ifTop (return ()) <|>)
    parsePath' = parsePath :: m (Maybe id)


------------------------------------------------------------------------------
-- | Retrieve the remaining path info and parse it into the identifier type.
parsePath :: (MonadSnap m, FromPath id) => m (Maybe id)
parsePath = fromPath . rqPathInfo <$> getRequest

