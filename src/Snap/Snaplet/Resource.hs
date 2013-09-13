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
import qualified Network.HTTP.Accept.MediaType as MT

------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString.Lazy (toStrict)
import Snap.Accept          (accepts)
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config
import Snap.Snaplet.Resource.Media
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
serveCode :: MonadSnap m => Int -> m () -> m a
serveCode code handler = do
    modifyResponse (setResponseCode code)
    handler
    withResponse finishWith


------------------------------------------------------------------------------
serveFailure :: MonadSnap m => ResourceConfig m -> m ()
serveFailure cfg = serveCode 406 $ onServeFailure cfg


------------------------------------------------------------------------------
receiveFailure :: MonadSnap m => ResourceConfig m -> m a
receiveFailure cfg = serveCode 415 $ onReceiveFailure cfg


------------------------------------------------------------------------------
data Resource r = Resource


------------------------------------------------------------------------------
serve
    :: forall m r. (HasResourceConfig m, MonadSnap m, Media r, Stored r)
    => Resource r -> m ()
serve r = resourceConfig >>= serveWith r


------------------------------------------------------------------------------
serveWith :: forall m r.
    (MonadSnap m, Media r, Stored r) => Resource r -> ResourceConfig m -> m ()
serveWith _ cfg = method GET (retrieve (Info 0) >>= provide)
    <|> method POST (receive >>= store)
    <|> method PUT (receive >> update')
    <|> method DELETE (receive >>= delete)
    <|> method PATCH (receive >> update')
    <|> method OPTIONS undefined
  where
    provide = provideResource cfg :: r -> m ()
    receive = receiveResource cfg :: m r
    update' = update (Diff :: Diff r)



------------------------------------------------------------------------------
provideResource :: (MonadSnap m, Media r) => ResourceConfig m -> r -> m ()
provideResource cfg r =
    accepts (map (fmap provideWith) representations) <|> serveFailure cfg
  where provideWith f = writeBS $ f r


------------------------------------------------------------------------------
receiveResource :: (MonadSnap m, Media r) => ResourceConfig m -> m r
receiveResource cfg = (<|> receiveFailure cfg) $ do
    req <- getRequest
    case getHeader "Content-Type" req >>= MT.parse of
        Nothing    -> pass
        Just ctype -> do
            body <- toStrict <$> readRequestBody (maxRequestBodySize cfg)
            maybe pass return $ lookup ctype parsers >>= ($ body)

