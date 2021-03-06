------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Failure
    ( headerFailure
    , pathFailure
    , queryFailure
    , lookupFailure
    , methodFailure
    , acceptFailure
    , contentTypeFailure
    , contentParseFailure
    ) where

------------------------------------------------------------------------------
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Config
import Snap.Snaplet.Rest.Options
import Snap.Snaplet.Rest.Resource


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
headerFailure :: MonadSnap m => ResourceConfig m -> m a
headerFailure = failure 400 . onHeaderFailure


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
pathFailure :: MonadSnap m => ResourceConfig m -> m a
pathFailure = failure 400 . onPathFailure


------------------------------------------------------------------------------
-- | Serves a 400 error and runs the handler specified in the configuration.
queryFailure :: MonadSnap m => ResourceConfig m -> m a
queryFailure = failure 400 . onQueryFailure


------------------------------------------------------------------------------
-- | Serves a 404 error and runs the handler specified in the configuration.
lookupFailure :: MonadSnap m => ResourceConfig m -> m a
lookupFailure = failure 404 . onLookupFailure


------------------------------------------------------------------------------
-- | Serves a 405 error and runs the handler specified in the configuration,
-- specifying which methods are allowed in the Allow header.
methodFailure
    :: MonadSnap m => Resource res m id diff -> ResourceConfig m -> m a
methodFailure res cfg = do
    setAllow (optionsFor res)
    failure 405 $ onMethodFailure cfg


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
contentParseFailure :: MonadSnap m => ResourceConfig m -> m a
contentParseFailure = failure 400 . onContentParseFailure


------------------------------------------------------------------------------
-- | Serve the given error code, running the given handler.
failure :: MonadSnap m => Int -> m () -> m a
failure code handler = do
    modifyResponse (setResponseCode code)
    handler
    withResponse finishWith

