------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Config
    (
    -- * Configuration
      ResourceConfig (..)
    , defaultConfig

    -- * Snaplet type class
    , HasResourceConfig (..)
    ) where

------------------------------------------------------------------------------
import Data.Int  (Int64)


------------------------------------------------------------------------------
-- | Configuration data.
data ResourceConfig m = ResourceConfig
    {
    -- | Action to run if the request header parsing fails.
      onHeaderFailure :: m ()

    -- | Action to run if the resource path parsing fails.
    , onPathFailure :: m ()

    -- | Action to run if the requested resource cannot be found.
    , onLookupFailure :: m ()

    -- | Action to run if the response media type is not supported.
    , onAcceptFailure :: m ()

    -- | Action to run if the request media type is not supported.
    , onContentTypeFailure :: m ()

    -- | Action to run if the request body parse fails.
    , onRequestFailure :: m ()

    -- | Maximum size of request bodies allowed when receiving resources.
    , maxRequestBodySize :: Int64
    }


------------------------------------------------------------------------------
-- | The default configuration settings.
--
-- > defaultConfig = ResourceConfig
-- >     { onServeFailure = return ()
-- >     , onReceiveFailure = return ()
-- >     , maxRequestBodySize = 8192
-- >     }
defaultConfig :: Monad m => ResourceConfig m
defaultConfig = ResourceConfig
    { onHeaderFailure = return ()
    , onPathFailure = return ()
    , onLookupFailure = return ()
    , onAcceptFailure = return ()
    , onContentTypeFailure = return ()
    , onRequestFailure = return ()
    , maxRequestBodySize = 8192
    }


------------------------------------------------------------------------------
-- | The type class for an implementing Snaplet.
class HasResourceConfig m where
    -- | Retrieve the configuration from the Snaplet monad.
    resourceConfig :: m (ResourceConfig m)

