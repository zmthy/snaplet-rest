------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Rep
    ( Rep (..)
    ) where

------------------------------------------------------------------------------
import Data.ByteString               (ByteString)
import Network.HTTP.Accept.MediaType (MediaType)


------------------------------------------------------------------------------
class Rep r where
    toRep   :: [(MediaType, r -> ByteString)]
    fromRep :: [(MediaType, ByteString -> Maybe r)]

