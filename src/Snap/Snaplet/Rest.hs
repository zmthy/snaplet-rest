------------------------------------------------------------------------------
module Snap.Snaplet.Rest
    (
    -- * Serving resources
      serveResource
    , serveResourceWith

    -- * Resource
    , Resource
    , resource
    , addMedia
    , setCreate
    , setRead
    , setUpdate
    , setDelete
    , setToDiff
    , setFromParams
    , setPutAction
    , PutAction (..)

    -- * Request parsing
    , FromRequest (..)
    , parseRead
    , Params

    -- * Media
    , Media
    , newMedia
    , newIntermediateMedia
    , newRequestMedia
    , newResponseMedia
    , MediaSetter
    , fromResource
    , toResource
    , toDiff
    , toEither
    , fromResourceList
    , toResourceList

    -- * Common media instances
    , json
    , jsonFromInstances
    , xml
    , xhtml
    , html
    , form
    , multipart

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

