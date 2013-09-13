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
    :: (HasResourceConfig m, MonadSnap m, Media r, FromPath i, Stored m r i)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
-- | Serve the specified resource using the given configuration.
serveWith
    :: forall m r i. (MonadSnap m, Media r, FromPath i, Stored m r i)
    => Resource r -> ResourceConfig m -> m ()
serveWith _ cfg =
    method GET (getResource cfg provide)
    <|> method POST (receive >>= store)
    <|> method PUT (receive >> update')
    <|> method DELETE (receive >>= delete)
    <|> method PATCH (receive >> update')
    <|> method OPTIONS undefined
  where
    provide = serveMediaWith cfg :: r -> m ()
    receive = receiveMediaWith cfg :: m r
    update' = update (Diff :: Diff r)


------------------------------------------------------------------------------
-- | Parses the remaining path information to produce identifying information
-- and retrieve the desired resource and serve it the client with the given
-- function.
getResource :: (MonadSnap m, FromPath i, Stored m r i)
    => ResourceConfig m -> (r -> m ()) -> m ()
getResource cfg provide = do
    req <- getRequest
    maybe pathParseError (retrieve >=> maybe (lookupFailure cfg) provide)
        (fromPath $ rqPathInfo req)
  where
    pathParseError = error "Path parse error"

