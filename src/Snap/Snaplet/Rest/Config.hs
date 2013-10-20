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
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Control.Monad.State (get)
import Data.Int            (Int64)
import Data.Text           (Text)
import Snap.Core
import Snap.Snaplet


------------------------------------------------------------------------------
-- | Configuration data.
data ResourceConfig m = ResourceConfig
    {
    -- | The maximum number of members to retrieve from a collection in
    -- a single request.
      readLimit :: Maybe Int

    -- | Maximum size of request bodies allowed when receiving resources.
    , maxRequestBodySize :: Int64

    -- | Action to run if the request header parsing fails.
    , onHeaderFailure :: m ()

    -- | Action to run if the resource path parsing fails.
    , onPathFailure :: m ()

    -- | Action to run if the URL query string parsing fails.
    , onQueryFailure :: m ()

    -- | Action to run if the requested resource cannot be found.
    , onLookupFailure :: m ()

    -- | Action to run an invalid method is requested on a resource.
    , onMethodFailure :: m ()

    -- | Action to run if the response media type is not supported.
    , onAcceptFailure :: m ()

    -- | Action to run if the request media type is not supported.
    , onContentTypeFailure :: m ()

    -- | Action to run if the request body parse fails.
    , onContentParseFailure :: m ()

    }


------------------------------------------------------------------------------
-- | The default configuration settings.  Requires a value for the maximum
-- size of a request body.
--
-- > defaultConfig mrbs = ResourceConfig
-- >     { readLimit = Nothing
-- >     , maxRequestBodySize = mrbs
-- >     , on*Failure = write "reason"
-- >     }
defaultConfig :: MonadSnap m => Int64 -> ResourceConfig m
defaultConfig mrbs = ResourceConfig
    { readLimit             = Nothing
    , maxRequestBodySize    = mrbs
    , onHeaderFailure       = write "Failed to parse request headers\n"
    , onPathFailure         = write "Failed to parse resource path\n"
    , onQueryFailure        = write "Failed to parse query string\n"
    , onLookupFailure       = write "Failed to find resource\n"
    , onMethodFailure       = write "Method not allowed\n"
    , onAcceptFailure       = write "No required media types are supported\n"
    , onContentTypeFailure  = write "No parser for request content available\n"
    , onContentParseFailure = write "Failed to parse request content\n"
    }
  where
    write msg = do
        modifyResponse $ setContentType "text/plain" .
            setContentLength (fromIntegral $ BS.length msg)
        writeBS msg


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
resourceInitDefault :: Int64 -> SnapletInit b (Resources b)
resourceInitDefault mrbs =
    makeSnaplet snapletName snapletDescription Nothing $
        return $ defaultConfig mrbs


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

