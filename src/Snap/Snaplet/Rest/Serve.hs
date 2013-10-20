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
import Snap.Snaplet.Rest.Failure
import Snap.Snaplet.Rest.FromRequest.Internal
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Options
import Snap.Snaplet.Rest.Proxy                (Proxy (..))
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serveResource
    :: (HasResourceConfig b, FromRequest id)
    => Resource res (Handler b b) id diff -> Handler b b ()
serveResource res = getResourceConfig >>= serveResourceWith res


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m -> m ()
serveResourceWith res' cfg = checkPath $
        serveRoute' GET (handleGet res) (retrieve res)
    <|> serveRoute' POST (handlePost res) (create res)
    <|> serveRoute' DELETE (handleDelete res) (delete res)
    <|> servePut res cfg
    <|> serveRoute' PATCH (handlePatch res) (update res)
    <|> serveRoute' OPTIONS (retrieveOptions parsePath) (Just res)
    <|> serveRoute' HEAD (handleGet res) (retrieve res)
  where
    res = complete res'
    serveRoute' = serveRoute res cfg
    checkPath = if pathEnabled' res Proxy
        then id else (ifNotTop (pathFailure cfg) <|>)


------------------------------------------------------------------------------
-- | Serves a route for the given method if the Maybe value is Just, otherwise
-- serves a method failure error.
serveRoute
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> Method
    -> (ResourceConfig m -> a -> m b) -> Maybe a -> m b
serveRoute res cfg mt rt mf = method mt $
    maybe (methodFailure res cfg) (rt cfg) mf


------------------------------------------------------------------------------
-- | Produces a PUT response depending on the PutAction in the resource.
servePut
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m -> m ()
servePut res cfg =
    (ifTop (serveRoute' (replaceResources res) (both create delete)) <|>) $
    ifNotTop $ case putAction res of
        Just Create -> serveRoute' (createResource res) (create res)
        Just Update -> serveRoute' updatePut (both update toDiff)
        Nothing     -> serveRoute' (tryUpdateResource res)
            ((,,) <$> create res <*> update res <*> toDiff res)
  where
    serveRoute' = serveRoute res cfg PUT
    both f g = (,) <$> f res <*> g res
    updatePut _ (update', toDiff') = updateResource
        (fmap toDiff' . receive (parsers res)) cfg update'


------------------------------------------------------------------------------
-- | Routes to 'retrieveResources' and 'retrieveResource'.
handleGet
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m -> (id -> m [res])
    -> m ()
handleGet res cfg retrieve' =
    ifTop (retrieveResources res cfg retrieve') <|> retrieveResource res cfg retrieve'


------------------------------------------------------------------------------
-- | Retrieves and serves resources using the URL query string.
retrieveResources
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> (id -> m [res])
    -> m ()
retrieveResources res cfg retrieve' = parseParams res cfg >>= maybe
    (queryFailure cfg) (retrieve' >=> serve (listRenderers res) cfg . limit)
  where limit = maybe id take $ readLimit cfg


------------------------------------------------------------------------------
-- | Retrieve and serve a resource using the remaining path information.
retrieveResource
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m -> (id -> m [res])
    -> m ()
retrieveResource res cfg retrieve' = parsePath >>= maybe (pathFailure cfg)
    (retrieve' >=> maybe (lookupFailure cfg)
        (serve (renderers res) cfg) . listToMaybe)


------------------------------------------------------------------------------
-- | Routes to 'createResource' if there is no remaining path information,
-- otherwise indicates that POST is not allowed directly on a resource.
handlePost
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> (res -> m ()) -> m ()
handlePost res cfg create' =
    ifTop (createResource res cfg create') <|> methodFailure res cfg


------------------------------------------------------------------------------
-- | Create a new resource from the request body.
createResource
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> (res -> m ()) -> m ()
createResource res cfg create' = receive (parsers res) cfg >>= create'


------------------------------------------------------------------------------
-- | Replaces all matched resources with the ones from the request body.
-- Retrieves and parses the request body before proceeding to delete.  Note
-- that this operation is non-atomic.
replaceResources
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m
    -> (res -> m (), id -> m Bool) -> m ()
replaceResources res cfg (create', delete') = do
    m <- receive (listParsers res) cfg
    deleteResources res cfg delete'
    mapM_ create' m


------------------------------------------------------------------------------
-- | Updates resources from the request body.
updateResources
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
updateResources res cfg update' = do
    search <- parseParams res cfg >>= maybe (queryFailure cfg) return
    receive (diffParsers res) cfg >>= void . update' search


------------------------------------------------------------------------------
-- | Update a resource from the request body.  Sends a lookup failure if the
-- update failed.
updateResource
    :: (MonadSnap m, FromRequest id)
    => (ResourceConfig m -> m diff) -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
updateResource receive' cfg update' = receive' cfg >>=
    updateResource' cfg update' >>= flip unless (lookupFailure cfg)


------------------------------------------------------------------------------
-- | Update a resource with the given value.  Returns 'True' if the update was
-- successful.
updateResource'
    :: (MonadSnap m, FromRequest id)
    => ResourceConfig m -> (id -> diff -> m Bool) -> diff -> m Bool
updateResource' cfg update' diff = parsePath >>=
    maybe (pathFailure cfg) (`update'` diff)


------------------------------------------------------------------------------
-- | Attempts to update with the request body, and creates it instead if that
-- fails.
tryUpdateResource
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m
    -> (res -> m (), id -> diff -> m Bool, res -> diff) -> m ()
tryUpdateResource res cfg (create', update', toDiff') = do
    par <- receive (parsers res) cfg
    updateResource' cfg update' (toDiff' par) >>= flip unless (create' par)


------------------------------------------------------------------------------
-- | Routes to 'updateResources' and 'updateResource', ensuring that PATCH is
-- not disabled in the latter case.
handlePatch
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m
    -> (id -> diff -> m Bool) -> m ()
handlePatch res cfg update' =
    ifTop (updateResources res cfg update') <|> do
        unless (patchEnabled res) $ methodFailure res cfg
        updateResource (receive $ diffParsers res) cfg update'


------------------------------------------------------------------------------
-- | Routes to 'deleteResources' and 'deleteResource'.
handleDelete
    :: (MonadSnap m, FromRequest id)
    => Resource res m id diff -> ResourceConfig m -> (id -> m Bool) -> m ()
handleDelete res cfg delete' =
    ifTop (deleteResources res cfg delete') <|> deleteResource cfg delete'


------------------------------------------------------------------------------
-- | Deletes the resources matching the URL query string.
deleteResources
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> (id -> m Bool)
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
retrieveOptions
    :: MonadSnap m
    => m (Maybe id) -> ResourceConfig m
    -> Resource res m id diff -> m ()
retrieveOptions parsePath' cfg res = do
    isTop >>=
        flip unless (isNothing <$> parsePath' >>= flip when (pathFailure cfg))
    setAllow $ optionsFor res
    modifyResponse $ setContentLength 0


------------------------------------------------------------------------------
-- | Retrieve the remaining path info and parse it into the identifier type.
parsePath :: (MonadSnap m, FromRequest id) => m (Maybe id)
parsePath = fromPath . rqPathInfo <$> getRequest


------------------------------------------------------------------------------
-- | Retrieve the URL query string and parse it into the identifier type.
parseParams
    :: MonadSnap m
    => Resource res m id diff -> ResourceConfig m -> m (Maybe id)
parseParams res cfg = maybe (methodFailure res cfg) tryParse (fromParams res)
  where tryParse fromParams' = fromParams' . rqParams <$> getRequest


------------------------------------------------------------------------------
-- | Grabs the appropriate instance of 'pathEnabled' for the given resource.
pathEnabled'
    :: FromRequest id => Resource res m id diff -> Proxy id -> Bool
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

