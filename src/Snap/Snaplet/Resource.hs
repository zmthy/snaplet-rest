{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource
    (
      ParseMedia (..)
    , Media (..)
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
import Snap.Snaplet.Resource.Path
import Snap.Snaplet.Resource.Proxy
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serve
    :: (HasResourceConfig m, MonadSnap m,
        Media r, FromPath i, ParseMedia d, Diff r d, Stored m r i d)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveWith
    :: forall m r i d.  (MonadSnap m,
        Media r, FromPath i, ParseMedia d, Diff r d, Stored m r i d)
    => Resource r -> ResourceConfig m -> m ()
serveWith r cfg =
    methods [GET, HEAD] (getResource cfg (serveMediaWith cfg :: r -> m ()))
    <|> method POST ((receiveMediaWith cfg :: m r) >>= store)
    <|> method PUT ((receiveMediaWith cfg :: m r) >>= updateR . toDiff)
    <|> method DELETE (deleteResource r cfg)
    <|> method PATCH ((receiveMediaWith cfg :: m d) >>= updateR)
    {-<|> method OPTIONS undefined-}
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

