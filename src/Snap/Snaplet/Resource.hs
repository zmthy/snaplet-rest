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
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config
import Snap.Snaplet.Resource.Media
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
-- | A proxy type to indicate to 'serve' and 'serveWith' what resource to work
-- with, as there are no explicit indicators in their type.
--
-- For instance, to serve the Person type as a resource:
--
-- > serve (Resource :: Resource Person)
data Resource r = Resource


------------------------------------------------------------------------------
-- | Serve the specified resource using the configuration in the monad.
serve
    :: (HasResourceConfig m, MonadSnap m, Media r, Stored r)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveWith
    :: forall m r. (MonadSnap m, Media r, Stored r)
    => Resource r -> ResourceConfig m -> m ()
serveWith _ cfg = method GET (retrieve (Info 0) >>= provide)
    <|> method POST (receive >>= store)
    <|> method PUT (receive >> update')
    <|> method DELETE (receive >>= delete)
    <|> method PATCH (receive >> update')
    <|> method OPTIONS undefined
  where
    provide = serveMediaWith cfg :: r -> m ()
    receive = receiveMediaWith cfg :: m r
    update' = update (Diff :: Diff r)

