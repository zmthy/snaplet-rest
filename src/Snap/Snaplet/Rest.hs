------------------------------------------------------------------------------
module Snap.Snaplet.Rest
    (
    -- * Serving resources
      serveResource
    , serveResourceWith

    -- * Resource
    , Resource

    -- * Request parsing
    , FromRequest (..)
    , parseRead
    , Params

    -- * Config
    , ResourceConfig (..)
    , defaultConfig
    , HasResourceConfig (..)
    , Resources
    , resourceInit
    , resourceInitDefault
    ) where

------------------------------------------------------------------------------
import Snap.Core (Params)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Config
import Snap.Snaplet.Rest.FromRequest
import Snap.Snaplet.Rest.Resource
import Snap.Snaplet.Rest.Serve

