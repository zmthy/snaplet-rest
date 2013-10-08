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
    , ConstantResource
    , StoredResource
    , SplitResource

    -- * Builder
    , Builder.ResourceBuilder
    , Builder.buildResource
    , Builder.exists
    , Builder.fetch
    , Builder.store
    , Builder.update
    , Builder.delete

    -- * Diff
    , Diff (..)

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
import Snap.Snaplet.Rest.Diff
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
    => SplitResource rep par (Handler b b) id diff -> Handler b b ()
serveResource res = getResourceConfig >>= serveResourceWith res


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveResourceWith
    :: forall rep par m id diff. (MonadSnap m, FromMedia par m, ToMedia rep m
    , FromPath id, FromMedia diff m, Diff par diff)
    => SplitResource rep par m id diff -> ResourceConfig m -> m ()
serveResourceWith res cfg =
    serveRoute [GET] cfg fetchResourceWith (fetch res) $
    serveRoute [POST] cfg storeResourceWith (store res) $
    serveRoute [PUT, PATCH] cfg (updateResourceWith toDiff') (update res) $
    serveRoute [DELETE] cfg deleteResourceWith (delete res) $
    serveRoute [HEAD] cfg checkResourceWith exists' $
    serveRoute [OPTIONS] cfg fetchOptionsWith exists' pass
  where
    exists' = exists res <|> fmap (fmap isJust .) (fetch res)
    toDiff' = toDiff :: par -> diff


------------------------------------------------------------------------------
serveRoute
    :: MonadSnap m => [Method] -> ResourceConfig m
    -> (ResourceConfig m -> a -> m b) -> Maybe a -> m b -> m b
serveRoute ms cfg rt mf = maybe id (\f -> (methods ms (rt cfg f) <|>)) mf


------------------------------------------------------------------------------
-- | Fetch and serve a resource using the remaining path information.
fetchResourceWith
    :: (MonadSnap m, ToMedia rep m, FromPath id)
    => ResourceConfig m -> (id -> m (Maybe rep)) -> m ()
fetchResourceWith cfg fetch' = getRequest >>= maybe (pathFailure cfg)
    (fetch' >=> maybe (lookupFailure cfg) (serveMediaWith cfg))
    . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Store a new resource from the request body.
storeResourceWith
    :: (MonadSnap m, FromMedia par m)
    => ResourceConfig m -> (par -> m ()) -> m ()
storeResourceWith cfg store' = ifTop (receiveMediaWith cfg >>= store')
    <|> methodFailure cfg


------------------------------------------------------------------------------
-- | Update a resource from the request body.
updateResourceWith
    :: (MonadSnap m, FromMedia par m, FromPath id, FromMedia diff m)
    => (par -> diff) -> ResourceConfig m -> (id -> diff -> m ()) -> m ()
updateResourceWith toDiff' cfg update' =
    (method PUT (toDiff' <$> (receiveMediaWith cfg))
        <|> method PATCH (receiveMediaWith cfg))
    >>= updateResourceWith' cfg update'


------------------------------------------------------------------------------
-- | Unrouted form of 'updateResourceWith'.
updateResourceWith'
    :: (MonadSnap m, FromPath id)
    => ResourceConfig m -> (id -> diff -> m ()) -> diff -> m ()
updateResourceWith' cfg update' diff = getRequest >>=
    maybe (pathFailure cfg) (flip update' diff) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Delete a resource.
deleteResourceWith
    :: (MonadSnap m, FromPath id) => ResourceConfig m -> (id -> m ()) -> m ()
deleteResourceWith cfg delete' = getRequest >>=
    maybe (pathFailure cfg) delete' . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Similar to 'fetchResource', but only checks if the resource exists,
-- serving an empty body.
checkResourceWith
    :: (MonadSnap m, FromPath id)
    => ResourceConfig m -> (id -> m Bool) -> m ()
checkResourceWith cfg exists' = getRequest >>= maybe (pathFailure cfg)
    (exists' >=> flip when (lookupFailure cfg) . not) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
fetchOptionsWith
    :: (MonadSnap m, FromPath id)
    => ResourceConfig m -> (id -> m Bool) -> m ()
fetchOptionsWith cfg e = ifTop (serveMediaWith cfg CollectionOptions) <|> do
    getRequest >>= maybe (pathFailure cfg) return . fromPath . rqPathInfo >>=
        e >>= flip when (lookupFailure cfg) . not
    serveMediaWith cfg ResourceOptions

