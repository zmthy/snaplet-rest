------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource
    (
    -- * Resource
      Resource
    , resource
    , addMedia
    , setCreate
    , setRead
    , setUpdate
    , setDelete
    , setToDiff
    , setFromParams

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

    -- * Common media instances
    , json
    , jsonInstances
    , xml
    , xhtml
    , html
    , form
    , multipart
    ) where

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Builder
import Snap.Snaplet.Rest.Resource.Internal hiding (toDiff)
import Snap.Snaplet.Rest.Resource.Media

