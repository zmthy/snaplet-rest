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
    -- | Action to run if the resource serving fails.
      onServeFailure :: m ()

    -- | Action to run if the resource receiving fails.
    , onReceiveFailure :: m ()

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
    { onServeFailure = return ()
    , onReceiveFailure = return ()
    , maxRequestBodySize = 8192
    }


------------------------------------------------------------------------------
-- | The type class for an implementing Snaplet.
class HasResourceConfig m where
    -- | Retrieve the configuration from the Snaplet monad.
    resourceConfig :: m (ResourceConfig m)

