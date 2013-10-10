------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Config
    (
    -- * Configuration
      ResourceConfig (..)
    , defaultConfig

    -- * Snaplet type class
    , HasResourceConfig (..)
    , Resources
    , resourceInit
    , resourceInitDefault

    -- * Local utility
    , getResourceConfig
    ) where

------------------------------------------------------------------------------
import Control.Monad.State (get)
import Data.Int            (Int64)
import Data.Text           (Text)
import Snap.Snaplet


------------------------------------------------------------------------------
-- | Configuration data.
data ResourceConfig m = ResourceConfig
    {
    -- | Maximum size of request bodies allowed when receiving resources.
      maxRequestBodySize :: Int64

    -- | Action to run if the request header parsing fails.
    , onHeaderFailure :: m ()

    -- | Action to run if the resource path parsing fails.
    , onPathFailure :: m ()

    -- | Action to run if the requested resource cannot be found.
    , onLookupFailure :: m ()

    -- | Action to run an invalid method is requested on a resource.
    , onMethodFailure :: m ()

    -- | Action to run if the response media type is not supported.
    , onAcceptFailure :: m ()

    -- | Action to run if the request media type is not supported.
    , onContentTypeFailure :: m ()

    -- | Action to run if the request body parse fails.
    , onRequestFailure :: m ()

    }


------------------------------------------------------------------------------
-- | The default configuration settings.
--
-- > defaultConfig = ResourceConfig
-- >     { on*Failure = return ()
-- >     , maxRequestBodySize = 8192
-- >     }
defaultConfig :: Monad m => ResourceConfig m
defaultConfig = ResourceConfig
    { maxRequestBodySize   = 8192
    , onHeaderFailure      = return ()
    , onPathFailure        = return ()
    , onLookupFailure      = return ()
    , onMethodFailure      = return ()
    , onAcceptFailure      = return ()
    , onContentTypeFailure = return ()
    , onRequestFailure     = return ()
    }


------------------------------------------------------------------------------
-- | The type class for an implementing Snaplet.
class HasResourceConfig b where

    -- | Retrieve the configuration from the Snaplet monad.
    resourceLens :: SnapletLens (Snaplet b) (ResourceConfig (Handler b b))


------------------------------------------------------------------------------
-- | Convenience alias of 'ResourceConfig'.
type Resources b = ResourceConfig (Handler b b)


------------------------------------------------------------------------------
-- | Initialize the resource snaplet with the given configuration.
resourceInit
    :: ResourceConfig (Handler b b)
    -> SnapletInit b (Resources b)
resourceInit = makeSnaplet snapletName snapletDescription Nothing . return


------------------------------------------------------------------------------
-- | Initialize the resource snaplet with the default configuration.
resourceInitDefault :: SnapletInit b (Resources b)
resourceInitDefault = makeSnaplet snapletName snapletDescription Nothing $
    return defaultConfig


------------------------------------------------------------------------------
snapletName :: Text
snapletName = "rest-resources"


------------------------------------------------------------------------------
snapletDescription :: Text
snapletDescription = "REST resources"


------------------------------------------------------------------------------
-- | Returns the resource configuration.
getResourceConfig
    :: HasResourceConfig b => Handler b v (ResourceConfig (Handler b b))
getResourceConfig = withTop' resourceLens get

