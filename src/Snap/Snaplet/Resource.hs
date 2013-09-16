{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource
    (
      Media (..)
    , Stored (..)
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
import Snap.Snaplet.Resource.Failure
import Snap.Snaplet.Resource.Media
import Snap.Snaplet.Resource.Proxy
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serve
    :: (HasResourceConfig m, MonadSnap m, Media r, Stored m r i)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveWith
    :: forall m r i. (MonadSnap m, Media r, Stored m r i)
    => Resource r -> ResourceConfig m -> m ()
serveWith r cfg =
    method GET (getResource cfg (serveMediaWith cfg :: r -> m ()))
    <|> method POST (receive >>= store)
    <|> method PUT (receive >> update')
    <|> method DELETE (deleteResource r cfg)
    <|> method HEAD undefined
    <|> method PATCH (receive >> update')
    <|> method OPTIONS undefined
  where
    receive = receiveMediaWith cfg :: m r
    update' = update (Diff :: Diff r)


------------------------------------------------------------------------------
-- | Parses the remaining path information, producing identifying information
-- to retrieve the desired resource and serve it to the client with the given
-- function.
getResource
    :: (MonadSnap m, Stored m r i) => ResourceConfig m -> (r -> m ()) -> m ()
getResource cfg provide = getRequest >>= maybe (pathFailure cfg)
    (retrieve >=> maybe (lookupFailure cfg) provide) . fromPath . rqPathInfo


------------------------------------------------------------------------------
-- | Parses the remaining path information, producing identifying information
-- to delete the desired resource.
deleteResource
    :: (MonadSnap m, Stored m r i) => Resource r -> ResourceConfig m -> m ()
deleteResource r cfg = getRequest >>=
    maybe (pathFailure cfg) (delete r) . fromPath . rqPathInfo

