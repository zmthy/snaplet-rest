------------------------------------------------------------------------------
-- | Defines the 'Media' type class, which is used for converting back and
-- forth from media representations in HTTP.
module Snap.Snaplet.Resource.Media
    (
    -- * Type class
      Media (..)

    -- * Serving and receiving
    , serveMedia
    , serveMediaWith
    , receiveMedia
    , receiveMediaWith
    ) where

------------------------------------------------------------------------------
import qualified Network.HTTP.Accept.MediaType as MT

------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString               (ByteString)
import Data.ByteString.Lazy          (toStrict)
import Network.HTTP.Accept.MediaType (MediaType)
import Snap.Accept                   (accepts)
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config
import Snap.Snaplet.Resource.Failure


------------------------------------------------------------------------------
-- | Instances of this type class can be represented as various media types
-- for communication from the server to the client and back again.
-- Implementations specify the supported media types and associated conversion
-- methods.
--
-- Note that the conversion does not necessarily need to be a two-way process:
-- for instance, a type may have an HTML representation for output, but
-- a form-encoding parser for input.
class Media r where

    -- | The media types that this type can be represented as, and the
    -- functions to perform create each representation.
    representations :: [(MediaType, r -> ByteString)]

    -- | The media types that this type can be parsed from, and the functions
    -- to parse each representation.
    parsers         :: [(MediaType, ByteString -> Maybe r)]


------------------------------------------------------------------------------
-- | Serve the given media using the configuration in the monad.
serveMedia :: (HasResourceConfig m, MonadSnap m, Media r) => r -> m ()
serveMedia r = resourceConfig >>= flip serveMediaWith r


------------------------------------------------------------------------------
-- | Serve the given media using the given configuration.
serveMediaWith :: (MonadSnap m, Media r) => ResourceConfig m -> r -> m ()
serveMediaWith cfg r =
    accepts (map (fmap provideWith) representations) <|> serveFailure cfg
  where provideWith f = writeBS $ f r


------------------------------------------------------------------------------
-- | Receive media using the configureation in the monad.
receiveMedia :: (HasResourceConfig m, MonadSnap m, Media r) => m r
receiveMedia = resourceConfig >>= receiveMediaWith


------------------------------------------------------------------------------
-- | Receive media using the given configuration.
receiveMediaWith :: (MonadSnap m, Media r) => ResourceConfig m -> m r
receiveMediaWith cfg = (<|> receiveFailure cfg) $ do
    req <- getRequest
    case getHeader "Content-Type" req >>= MT.parse of
        Nothing    -> pass
        Just ctype -> do
            body <- toStrict <$> readRequestBody (maxRequestBodySize cfg)
            maybe pass return $ lookup ctype parsers >>= ($ body)

