{-# LANGUAGE FlexibleContexts #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Serve
    ( serveResource
    , serveResourceWith
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Maybe
import Snap.Core
import Snap.Snaplet        (Handler)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Config
import Snap.Snaplet.Rest.Diff.Internal        (Diff (..))
import Snap.Snaplet.Rest.Failure
import Snap.Snaplet.Rest.FromRequest.Internal
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Options
import Snap.Snaplet.Rest.Proxy                (Proxy (..))
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serveResource
    :: (HasResourceConfig b, FromMedia par (Handler b b)
    , FromMedia [par] (Handler b b), ToMedia rep (Handler b b)
    , ToMedia [rep] (Handler b b), FromRequest id
    , FromMedia diff (Handler b b), Diff par diff)
    => Resource rep par (Handler b b) id diff -> Handler b b ()
serveResource res = getResourceConfig >>= serveResourceWith res


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: (MonadSnap m, FromMedia par m, FromMedia [par] m, ToMedia rep m
    , ToMedia [rep] m, FromRequest id, FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> m ()
serveResourceWith res cfg = checkPath $
        serveRoute' GET (handleGet res) (fetch res)
    <|> serveRoute' POST (handlePost res) (store res)
    <|> serveRoute' DELETE (handleDelete res) (delete res)
    <|> servePut res cfg toDiff
    <|> serveRoute' PATCH (handlePatch res Proxy) (update res)
    <|> serveRoute' OPTIONS (fetchOptions parsePath Proxy) (Just res)
    <|> serveRoute' HEAD fetchResource (fetch res)
  where
    serveRoute' = serveRoute res cfg
    checkPath = if pathEnabled' res Proxy
        then id else (ifNotTop (pathFailure cfg) <|>)


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
    :: (MonadSnap m, FromMedia par m, FromMedia [par] m, FromMedia diff m
    , FromRequest id, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (par -> diff) -> m ()
servePut res cfg toDiff' =
    (ifTop (serveRoute' (replaceResources res) (both store delete)) <|>) $
    ifNotTop $ case putAction res of
        JustStore  -> serveRoute' storeResource (store res)
        JustUpdate -> serveRoute' (updateResource toDiff') (update res)
        TryUpdate  ->
            ifNotTop (serveRoute' tryUpdateResource (both store update))
  where
    serveRoute' = serveRoute res cfg PUT
    both f g = (,) <$> f res <*> g res


------------------------------------------------------------------------------
-- | Routes to 'fetchResources' and 'fetchResource'.
handleGet
    :: (MonadSnap m, ToMedia [rep] m, ToMedia rep m, FromRequest id
    , Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (id -> m [rep])
    -> m ()
handleGet res cfg fetch' =
    ifTop (fetchResources res cfg fetch') <|> fetchResource cfg fetch'


------------------------------------------------------------------------------
-- | Fetches and serves resources using the URL query string.
fetchResources
    :: (MonadSnap m, ToMedia [rep] m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (id -> m [rep])
    -> m ()
fetchResources res cfg fetch' = parseParams res cfg >>=
    maybe (queryFailure cfg) (fetch' >=> serveMediaWith cfg . limit)
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
-- | Routes to 'storeResource' if there is no remaining path information,
-- otherwise indicates that POST is not allowed directly on a resource.
handlePost
    :: (MonadSnap m, FromMedia par m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (par -> m ()) -> m ()
handlePost res cfg store' =
    ifTop (storeResource cfg store') <|> methodFailure res cfg


------------------------------------------------------------------------------
-- | Store a new resource from the request body.
storeResource
    :: (MonadSnap m, FromMedia par m)
    => ResourceConfig m -> (par -> m ()) -> m ()
storeResource cfg store' = receiveMediaWith cfg >>= store'


------------------------------------------------------------------------------
-- | Replaces all matched resources with the ones from the request body.
-- Retrieves and parses the request body before proceeding to delete.  Note
-- that this operation is non-atomic.
replaceResources
    :: (MonadSnap m, FromMedia [par] m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m
    -> (par -> m (), id -> m Bool) -> m ()
replaceResources res cfg (store', delete') = do
    media <- receiveMediaWith cfg
    deleteResources res cfg delete'
    mapM_ store' media


------------------------------------------------------------------------------
-- | Updates resources from the request body.
updateResources
    :: (MonadSnap m, FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
updateResources res cfg update' = do
    search <- parseParams res cfg >>= maybe (queryFailure cfg) return
    receiveMediaWith cfg >>= void . update' search


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
    maybe (pathFailure cfg) (`update'` diff)


------------------------------------------------------------------------------
-- | Attempts to update with the request body, and stores it instead if that
-- fails.
tryUpdateResource
    :: (MonadSnap m, FromMedia par m, FromRequest id, Diff par diff)
    => ResourceConfig m -> (par -> m (), id -> diff -> m Bool) -> m ()
tryUpdateResource cfg (store', update') = do
    par <- receiveMediaWith cfg
    updateResource' cfg update' (toDiff par) >>= flip unless (store' par)


------------------------------------------------------------------------------
-- | Routes to 'updateResources' and 'updateResource', ensuring that PATCH is
-- not disabled in the latter case.
handlePatch
    :: (MonadSnap m, FromRequest id, FromMedia diff m, Diff par diff)
    => Resource rep par m id diff -> Proxy (par, diff) -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
handlePatch res proxy cfg update' =
    ifTop (updateResources res cfg update') <|> do
        unless (patchEnabled proxy) $ methodFailure res cfg
        updateResource id cfg update'


------------------------------------------------------------------------------
-- | Routes to 'deleteResources' and 'deleteResource'.
handleDelete
    :: (MonadSnap m, FromRequest id, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (id -> m Bool) -> m ()
handleDelete res cfg delete' =
    ifTop (deleteResources res cfg delete') <|> deleteResource cfg delete'


------------------------------------------------------------------------------
-- | Deletes the resources matching the URL query string.
deleteResources
    :: (MonadSnap m, Diff par diff)
    => Resource rep par m id diff -> ResourceConfig m -> (id -> m Bool)
    -> m ()
deleteResources res cfg delete' = parseParams res cfg >>=
    maybe (queryFailure cfg) (void . delete')


------------------------------------------------------------------------------
-- | Deletes the resource at the current path.
deleteResource
    :: (MonadSnap m, FromRequest id)
    => ResourceConfig m -> (id -> m Bool) -> m ()
deleteResource cfg delete' = parsePath >>= maybe (pathFailure cfg)
    (delete' >=> flip unless (lookupFailure cfg))


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptions
    :: (MonadSnap m, Diff par diff)
    => m (Maybe id) -> Proxy (par, diff) -> ResourceConfig m
    -> Resource rep par m id diff -> m ()
fetchOptions parsePath' p cfg res = do
    isTop >>=
        flip unless (isNothing <$> parsePath' >>= flip when (pathFailure cfg))
    setAllow p $ optionsFor res
    modifyResponse $ setContentLength 0


------------------------------------------------------------------------------
-- | Retrieve the remaining path info and parse it into the identifier type.
parsePath :: (MonadSnap m, FromRequest id) => m (Maybe id)
parsePath = fromPath . rqPathInfo <$> getRequest


------------------------------------------------------------------------------
-- | Retrieve the URL query string and parse it into the identifier type.
parseParams
    :: (MonadSnap m, Diff par diff)
    => Resource res par m id diff -> ResourceConfig m -> m (Maybe id)
parseParams res cfg = maybe (methodFailure res cfg) tryParse (fromParams res)
  where tryParse fromParams' = fromParams' . rqParams <$> getRequest


------------------------------------------------------------------------------
-- | Grabs the appropriate instance of 'pathEnabled' for the given resource.
pathEnabled'
    :: FromRequest id => Resource rep par m id diff -> Proxy id -> Bool
pathEnabled' _ = pathEnabled


------------------------------------------------------------------------------
-- | Determines if 'rqPathInfo' is null.
isTop :: MonadSnap m => m Bool
isTop = BS.null . rqPathInfo <$> getRequest


------------------------------------------------------------------------------
-- | Opposite of 'ifTop', runs the given action to 'rqPathInfo' is not null.
ifNotTop :: MonadSnap m => m a -> m a
ifNotTop m = do
    top <- isTop
    if top then pass else m

