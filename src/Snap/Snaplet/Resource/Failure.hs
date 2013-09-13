------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Failure
    ( serveFailure
    , receiveFailure
    , lookupFailure
    ) where

------------------------------------------------------------------------------
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config


------------------------------------------------------------------------------
-- | Serves a 406 error and runs the handler specified in the configuration.
serveFailure :: MonadSnap m => ResourceConfig m -> m a
serveFailure cfg = failure 406 $ onServeFailure cfg


------------------------------------------------------------------------------
-- | Serves a 415 error and runs the handler specified in the configuration.
receiveFailure :: MonadSnap m => ResourceConfig m -> m a
receiveFailure cfg = failure 415 $ onReceiveFailure cfg


------------------------------------------------------------------------------
-- | Serves a 404 error and runs the handler specified in the configureation.
lookupFailure :: MonadSnap m => ResourceConfig m -> m a
lookupFailure cfg = failure 404 $ onLookupFailure cfg


------------------------------------------------------------------------------
-- | Serve the given error code, running the given handler.
failure :: MonadSnap m => Int -> m () -> m a
failure code handler = do
    modifyResponse (setResponseCode code)
    handler
    withResponse finishWith

