------------------------------------------------------------------------------
-- | Defines the 'Media' type class, which is used for converting back and
-- forth from media representations in HTTP.
module Snap.Snaplet.Resource.Media
    ( Media (..)
    ) where

------------------------------------------------------------------------------
import Data.ByteString               (ByteString)
import Network.HTTP.Accept.MediaType (MediaType)


------------------------------------------------------------------------------
-- | Instances of this type class can be represented as various media types
-- for communication from the server to the client and back again.
-- Implementations specify the supported media types and associated conversion
-- methods.
--
-- Note that the conversion does not necessarily need to be a two-way process:
-- for instance, a type may have an HTML representation for output, but
-- a form-encoding parser for input.
class Media r where

    -- | The media types that this type can be represented as, and the
    -- functions to perform create each representation.
    representations :: [(MediaType, r -> ByteString)]

    -- | The media types that this type can be parsed from, and the functions
    -- to parse each representation.
    parsers         :: [(MediaType, ByteString -> Maybe r)]

