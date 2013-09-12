{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Resource
    (
      Rep (..)
    , Stored (..)
    , HasResourceConfig (..)
    , ResourceConfig (..)
    , defaultConfig

    -- * Serving resources
    , serveResource
    , receiveResource
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
import Snap.Snaplet.Resource.Rep
import Snap.Snaplet.Resource.Stored


------------------------------------------------------------------------------
serveCode
    :: (HasResourceConfig m, MonadSnap m)
    => Int -> (ResourceConfig m -> m ()) -> m a
serveCode code handler = do
    modifyResponse (setResponseCode code)
    resourceConfig >>= handler
    withResponse finishWith


------------------------------------------------------------------------------
serveFailure :: (HasResourceConfig m, MonadSnap m) => m ()
serveFailure = serveCode 406 onServeFailure


------------------------------------------------------------------------------
receiveFailure :: (HasResourceConfig m, MonadSnap m) => m a
receiveFailure = serveCode 415 onReceiveFailure


------------------------------------------------------------------------------
serveResource :: forall m r.
    (HasResourceConfig m, MonadSnap m, Rep r, Stored r) => m r
serveResource = (>> withResponse finishWith) $ foldr1 (<|>)
    [ method GET $ retrieve (Info 0) >>= provide
    , method POST $ receive >>= store
    , method PUT $ receive >> update'
    , method DELETE $ receive >>= delete
    , method PATCH $ receive >> update'
    , method OPTIONS undefined
    ]
  where
    provide = provideResource :: r -> m ()
    receive = receiveResource :: m r
    update' = update (Diff :: Diff r)


------------------------------------------------------------------------------
provideResource :: (HasResourceConfig m, MonadSnap m, Rep r) => r -> m ()
provideResource r = accepts (map (fmap serveWith) toRep) <|> serveFailure
  where serveWith f = writeBS $ f r


------------------------------------------------------------------------------
receiveResource :: (HasResourceConfig m, MonadSnap m, Rep r) => m r
receiveResource = (<|> receiveFailure) $ do
    req <- getRequest
    cfg <- resourceConfig
    case getHeader "Content-Type" req >>= MT.parse of
        Nothing    -> pass
        Just ctype -> do
            body <- toStrict <$> readRequestBody (maxRequestBodySize cfg)
            maybe pass return $ lookup ctype fromRep >>= ($ body)

