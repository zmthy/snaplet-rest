{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource
    (
      FromMedia (..)
    , ToMedia (..)
    , Stored (..)
    , Diff (..)
    , HasResourceConfig (..)
    , ResourceConfig (..)
    , defaultConfig

    -- * Serving resources
    , serve
    , serveWith
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
serve
    :: (HasResourceConfig m, MonadSnap m, FromMedia r, ToMedia r,
        FromPath i, FromMedia d, Diff r d, Stored m r i d)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveWith
    :: forall m r i d.  (MonadSnap m, FromMedia r, ToMedia r,
        FromPath i, FromMedia d, Diff r d, Stored m r i d)
    => Resource r -> ResourceConfig m -> m ()
serveWith r cfg =
    method GET (getResource cfg (serveMediaWith cfg :: r -> m ()))
    <|> method POST (ifTop $ (receiveMediaWith cfg :: m r) >>= store)
    <|> method PUT ((receiveMediaWith cfg :: m r) >>= updateR . toDiff)
    <|> method DELETE (deleteResource r cfg)
    <|> method PATCH ((receiveMediaWith cfg :: m d) >>= updateR)
    <|> method HEAD (checkResource r cfg)
    <|> method OPTIONS (getOptions r cfg)
  where updateR = updateResource r cfg :: d -> m ()


------------------------------------------------------------------------------
-- | Parses the remaining path information, producing identifying information
-- to retrieve the desired resource and serve it to the client with the given
-- function.
getResource
    :: (MonadSnap m, FromPath i, Stored m r i d)
    => ResourceConfig m -> (r -> m ()) -> m ()
getResource cfg provide = getRequest >>= maybe (pathFailure cfg)
    (retrieve >=> maybe (lookupFailure cfg) provide) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Parses the remaining path information, producing identifying information
-- to delete the desired resource.
deleteResource
    :: (MonadSnap m, FromPath i, Stored m r i d)
    => Resource r -> ResourceConfig m -> m ()
deleteResource r cfg = getRequest >>=
    maybe (pathFailure cfg) (delete r) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Parses the remaining path information, producing identifying information
-- to update the desired resource using the given diff data.
updateResource
    :: (MonadSnap m, FromPath i, Diff r d, Stored m r i d)
    => Resource r -> ResourceConfig m -> d -> m ()
updateResource r cfg diff = getRequest >>=
    maybe (pathFailure cfg) (flip (update r) diff) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Similar to 'getResource', but only checks if the resource exists, serving
-- an empty body.
checkResource
    :: (MonadSnap m, FromPath i, Stored m r i d)
    => Resource r -> ResourceConfig m -> m ()
checkResource r cfg = getRequest >>= maybe (pathFailure cfg)
    (exists r >=> flip when (lookupFailure cfg) . not) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Serves either collection or resource options, depending on the path.
getOptions
    :: (MonadSnap m, FromPath i, Stored m r i d)
    => Resource r -> ResourceConfig m -> m ()
getOptions r cfg = (ifTop (serveMediaWith cfg CollectionOptions) <|>) $ do
    getRequest >>= maybe (pathFailure cfg) return . fromPath . rqPathInfo >>=
        exists r >>= flip when (lookupFailure cfg) . not
    serveMediaWith cfg ResourceOptions

