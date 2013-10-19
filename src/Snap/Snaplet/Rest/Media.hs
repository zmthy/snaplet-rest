{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
-- | Defines type classes for converting back and forth from media
-- representations in HTTP.
--
-- Note that the conversion does not necessarily need to be a two-way process:
-- for instance, a type may have an HTML representation for output, but a
-- form-encoding parser for input.
module Snap.Snaplet.Rest.Media
    ( serve
    , receive
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Network.HTTP.Media.MediaType as MT

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.ByteString     (ByteString)
import Network.HTTP.Media  (MediaType, mapContent)
import Snap.Accept         (accepts)
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Config
import Snap.Snaplet.Rest.Failure


------------------------------------------------------------------------------
-- | Serve the given resource using the given configuration.
serve
    :: MonadSnap m
    => [(MediaType, a -> m ByteString)] -> ResourceConfig m -> a -> m ()
serve composers cfg rep =
        accepts (map (fmap $ writeDone <=< ($ rep)) $ composers)
    <|> acceptFailure cfg
  where
    writeDone bs = do
        modifyResponse $ setContentLength $ fromIntegral $ BS.length bs
        writeBS bs


------------------------------------------------------------------------------
receive
    :: MonadSnap m
    => [(MediaType, ByteString -> m (Maybe a))] -> ResourceConfig m -> m a
receive parsers cfg = do
    header <- getHeader "Content-Type" <$> getRequest
    ctype  <- mayFail headerFailure $ header >>= MT.parse
    parser <- mayFail contentTypeFailure $ mapContent ctype parsers
    body   <- LBS.toStrict <$> readRequestBody (maxRequestBodySize cfg)
    parser body >>= mayFail contentParseFailure
  where mayFail handler = maybe (handler cfg) return

