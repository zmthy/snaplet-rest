{-# LANGUAGE RankNTypes #-}

------------------------------------------------------------------------------
-- | Defines type classes for converting back and forth from media
-- representations in HTTP.
--
-- Note that the conversion does not necessarily need to be a two-way process:
-- for instance, a type may have an HTML representation for output, but a
-- form-encoding parser for input.
module Snap.Snaplet.Resource.Media
    (
    -- * Type classes
      FromMedia (..)
    , ToMedia (..)

    -- * Serving and receiving
    , serveMedia
    , serveMediaWith
    , receiveMedia
    , receiveMediaWith
    ) where

------------------------------------------------------------------------------
import qualified Network.HTTP.Media.MediaType as MT

------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Media   (MediaType, mapContent)
import Snap.Accept          (accepts)
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config
import Snap.Snaplet.Resource.Failure


------------------------------------------------------------------------------
-- | Instances of this type class can be parsed from various media types.
-- Implementations specify the supported media types and associated parsers.
class FromMedia r m where

    -- | The media types that this type can be parsed rfrom, and the functions
    -- to parse each representation.
    parsers :: [(MediaType, ByteString -> m (Maybe r))]


------------------------------------------------------------------------------
-- | Instances of this type class can be represented as various media types.
-- Implementations specify the supported media types and associated conversion
-- functions.
class ToMedia r m where

    -- | The media types that this type can be represented as, and the
    -- functions to perform create each representation.
    representations :: [(MediaType, r -> m ByteString)]


------------------------------------------------------------------------------
-- | Serve the given media using the configuration in the monad.
serveMedia :: (HasResourceConfig m, MonadSnap m, ToMedia r m) => r -> m ()
serveMedia r = resourceConfig >>= flip serveMediaWith r


------------------------------------------------------------------------------
-- | Serve the given media using the given configuration.
serveMediaWith :: (MonadSnap m, ToMedia r m) => ResourceConfig m -> r -> m ()
serveMediaWith cfg r =
    accepts (map (fmap provideWith) representations) <|> acceptFailure cfg
  where provideWith f = f r >>= writeBS


------------------------------------------------------------------------------
-- | Receive media using the configureation in the monad.
receiveMedia :: (HasResourceConfig m, MonadSnap m, FromMedia r m) => m r
receiveMedia = resourceConfig >>= receiveMediaWith


------------------------------------------------------------------------------
-- | Receive media using the given configuration.
receiveMediaWith :: (MonadSnap m, FromMedia r m) => ResourceConfig m -> m r
receiveMediaWith cfg = do
    header <- getHeader "Content-Type" <$> getRequest
    ctype  <- mayFail headerFailure $ header >>= MT.parse
    parser <- mayFail contentTypeFailure $ mapContent ctype parsers
    body   <- toStrict <$> readRequestBody (maxRequestBodySize cfg)
    parser body >>= mayFail requestFailure
  where mayFail handler = maybe (handler cfg) return

