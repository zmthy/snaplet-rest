------------------------------------------------------------------------------
module Snap.Snaplet.Rest
    (
    -- * Serving resources
      serveResource
    , serveResourceWith

    -- * Media
    , FromMedia (..)
    , ToMedia (..)

    -- * Resource
    , Resource

    -- * Builder
    , ResourceBuilder
    , buildResource
    , setFetch
    , setStore
    , setUpdate
    , setDelete
    , setFromParams
    , setPutAction

    -- * Request parsing
    , FromRequest (..)
    , parseRead
    , Params

    -- * Diff
    , Diff (toDiff)

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
import Snap.Snaplet.Rest.Diff
import Snap.Snaplet.Rest.FromRequest
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Resource
import Snap.Snaplet.Rest.Serve

