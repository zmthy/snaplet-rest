------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Failure
    ( headerFailure
    , pathFailure
    , lookupFailure
    , methodFailure
    , acceptFailure
    , contentTypeFailure
    , requestFailure
    ) where

------------------------------------------------------------------------------
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Config


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
headerFailure :: MonadSnap m => ResourceConfig m -> m a
headerFailure = failure 400 . onHeaderFailure


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
pathFailure :: MonadSnap m => ResourceConfig m -> m a
pathFailure = failure 400 . onPathFailure


------------------------------------------------------------------------------
-- | Serves a 404 error and runs the handler specified in the configuration.
lookupFailure :: MonadSnap m => ResourceConfig m -> m a
lookupFailure = failure 404 . onLookupFailure


------------------------------------------------------------------------------
-- | Serves a 405 error and runs the handler specified in the configuration.
methodFailure :: MonadSnap m => ResourceConfig m -> m a
methodFailure = failure 405 . onMethodFailure


------------------------------------------------------------------------------
-- | Serves a 406 error and runs the handler specified in the configuration.
acceptFailure :: MonadSnap m => ResourceConfig m -> m a
acceptFailure = failure 406 . onAcceptFailure


------------------------------------------------------------------------------
-- | Serves a 415 error and runs the handler specified in the configuration.
contentTypeFailure :: MonadSnap m => ResourceConfig m -> m a
contentTypeFailure = failure 415 . onContentTypeFailure


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
requestFailure :: MonadSnap m => ResourceConfig m -> m a
requestFailure = failure 400 . onRequestFailure


------------------------------------------------------------------------------
-- | Serve the given error code, running the given handler.
failure :: MonadSnap m => Int -> m () -> m a
failure code handler = do
    modifyResponse (setResponseCode code)
    handler
    withResponse finishWith

