------------------------------------------------------------------------------
-- | Specifies client options for given path point on the server.
module Snap.Snaplet.Resource.Options
    ( CollectionOptions (..)
    , ResourceOptions (..)
    ) where

------------------------------------------------------------------------------
import Snap.Snaplet.Resource.Media


------------------------------------------------------------------------------
-- | Options on a collection of resources.
data CollectionOptions = CollectionOptions

instance ToMedia CollectionOptions where
    representations =
        [ ("application/json", const "[]")
        , ("application/xml",  const "<options></options>")
        ]


------------------------------------------------------------------------------
-- | Options on a single resource.
data ResourceOptions = ResourceOptions

instance ToMedia ResourceOptions where
    representations =
        [ ("application/json", const "[]")
        , ("application/xml",  const "<options></options>")
        ]

