{-# LANGUAGE FlexibleContexts #-}

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

    -- * Request parsing
    , FromRequest (..)

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
import Snap.Snaplet.Rest.Diff.Internal     (Diff (..))
import Snap.Snaplet.Rest.Failure
import Snap.Snaplet.Rest.FromRequest
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Options
import Snap.Snaplet.Rest.Proxy             (Proxy (..))
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serveResource
    :: (HasResourceConfig b, FromMedia par (Handler b b)
    , ToMedia rep (Handler b b), ToMedia [rep] (Handler b b), FromRequest id
    , FromMedia diff (Handler b b), Diff par diff)
    => Resource rep par (Handler b b) id diff -> Handler b b ()
serveResource res = getResourceConfig >>= serveResourceWith res


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: (MonadSnap m, FromMedia par m, ToMedia rep m, ToMedia [rep] m
    , FromRequest id , FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> m ()
serveResourceWith res cfg =
        serveRoute' GET handleGet (fetch res)
    <|> serveRoute' POST (handlePost res) (store res)
    <|> serveRoute' DELETE deleteResource (delete res)
    <|> servePut res cfg toDiff
    <|> serveRoute' PATCH (handlePatch res Proxy) (update res)
    <|> serveRoute' OPTIONS (fetchOptions parsePath Proxy) (Just res)
    <|> serveRoute' HEAD fetchResource (fetch res)
  where serveRoute' = serveRoute res cfg


------------------------------------------------------------------------------
-- | Serves a route for the given method if the Maybe value is Just, otherwise
-- serves a method failure error.
serveRoute
    :: (MonadSnap m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> Method
    -> (ResourceConfig m -> a -> m b) -> Maybe a -> m b
serveRoute res cfg mt rt mf = method mt $
    maybe (methodFailure res cfg) (rt cfg) mf


------------------------------------------------------------------------------
-- | Produces a PUT response depending on the PutAction in the resource.
servePut
    :: (MonadSnap m, FromMedia par m, FromRequest id, FromMedia diff m
    , Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (par -> diff) -> m ()
servePut res cfg toDiff' = case (putAction res) of
    TryUpdate  -> serveRoute' handlePut $ (,) <$> store res <*> update res
    JustStore  -> serveRoute' storeResource (store res)
    JustUpdate -> serveRoute' (updateResource toDiff') (update res)
  where serveRoute' = serveRoute res cfg PUT


------------------------------------------------------------------------------
handleGet
    :: (MonadSnap m, ToMedia [rep] m, ToMedia rep m, FromRequest id)
    => ResourceConfig m -> (id -> m [rep]) -> m ()
handleGet cfg fetch' =
    ifTop (fetchResources cfg fetch') <|> fetchResource cfg fetch'


------------------------------------------------------------------------------
fetchResources
    :: (MonadSnap m, ToMedia [rep] m, FromRequest id)
    => ResourceConfig m -> (id -> m [rep]) -> m ()
fetchResources cfg fetch' = parseParams >>= maybe (queryFailure cfg)
    (fetch' >=> serveMediaWith cfg . limit)
  where
    limit = case fetchLimit cfg of
        Unlimited -> id
        LimitTo i -> take i


------------------------------------------------------------------------------
-- | Fetch and serve a resource using the remaining path information.
fetchResource
    :: (MonadSnap m, ToMedia rep m, FromRequest id)
    => ResourceConfig m -> (id -> m [rep]) -> m ()
fetchResource cfg fetch' = parsePath >>= maybe (pathFailure cfg)
    (fetch' >=> maybe (lookupFailure cfg) (serveMediaWith cfg) . listToMaybe)


------------------------------------------------------------------------------
-- | Store a new resource from the request body.
storeResource
    :: (MonadSnap m, FromMedia par m)
    => ResourceConfig m -> (par -> m ()) -> m ()
storeResource cfg store' = receiveMediaWith cfg >>= store'


------------------------------------------------------------------------------
-- | Routes to 'storeResource' if there is no remaining path information,
-- otherwise indicates that POST is not allowed directly on a resource.
handlePost
    :: (MonadSnap m, FromMedia par m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (par -> m ()) -> m ()
handlePost res cfg store' =
    ifTop (storeResource cfg store') <|> methodFailure res cfg


------------------------------------------------------------------------------
-- | Attempts to update with the request body, and stores it instead if that
-- fails.
handlePut
    :: (MonadSnap m, FromMedia par m, FromRequest id, Diff par diff)
    => ResourceConfig m -> ((par -> m ()), (id -> diff -> m Bool)) -> m ()
handlePut cfg (store', update') = do
    par <- receiveMediaWith cfg
    updateResource' cfg update' (toDiff par) >>= flip unless (store' par)


------------------------------------------------------------------------------
-- | Update a resource from the request body.  Sends a lookup failure if the
-- update failed.
updateResource
    :: (MonadSnap m, FromMedia par m, FromRequest id, FromMedia diff m)
    => (par -> diff) -> ResourceConfig m -> (id -> diff -> m Bool) -> m ()
updateResource toDiff' cfg update' = toDiff' <$> receiveMediaWith cfg
    >>= updateResource' cfg update' >>= flip unless (lookupFailure cfg)


------------------------------------------------------------------------------
-- | Update a resource with the given value.  Returns 'True' if the update was
-- successful.
updateResource'
    :: (MonadSnap m, FromRequest id)
    => ResourceConfig m -> (id -> diff -> m Bool) -> diff -> m Bool
updateResource' cfg update' diff = parsePath >>=
    maybe (pathFailure cfg) (flip update' diff)


------------------------------------------------------------------------------
-- | Ensures that PATCH is not disabled, then applies an update.
handlePatch
    :: (MonadSnap m, FromRequest id, FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> Proxy (par, diff) -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
handlePatch res proxy cfg update' = do
    when (patchDisabled proxy) $ methodFailure res cfg
    updateResource id cfg update'


------------------------------------------------------------------------------
-- | Delete a resource.
deleteResource
    :: (MonadSnap m, FromRequest id)
    => ResourceConfig m -> (id -> m Bool) -> m ()
deleteResource cfg delete' = parsePath >>= maybe (pathFailure cfg)
    (delete' >=> flip unless (lookupFailure cfg))


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptions
    :: (MonadSnap m, FromRequest id, Diff par diff)
    => m (Maybe id) -> Proxy (par, diff) -> ResourceConfig m
    -> Resource rep par m id diff -> m ()
fetchOptions parsePath' p cfg res = do
    ifNotTop $ isNothing <$> parsePath' >>= flip when (pathFailure cfg)
    setAllow p $ optionsFor res
    modifyResponse $ setContentLength 0
  where ifNotTop = (ifTop (return ()) <|>)


------------------------------------------------------------------------------
-- | Retrieve the remaining path info and parse it into the identifier type.
parsePath :: (MonadSnap m, FromRequest id) => m (Maybe id)
parsePath = fromPath . rqPathInfo <$> getRequest


------------------------------------------------------------------------------
-- | Retrieve the URL query string and parse it into the identifier type.
parseParams :: (MonadSnap m, FromRequest id) => m (Maybe id)
parseParams = fromParams . rqParams <$> getRequest

