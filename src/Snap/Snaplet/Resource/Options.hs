{-# LANGUAGE FlexibleInstances #-}

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

instance Monad m => ToMedia CollectionOptions m where
    representations =
        [ ("application/json", const $ return "[]")
        , ("application/xml",  const $ return "<options></options>")
        ]


------------------------------------------------------------------------------
-- | Options on a single resource.
data ResourceOptions = ResourceOptions

instance Monad m => ToMedia ResourceOptions m where
    representations =
        [ ("application/json", const $ return "[]")
        , ("application/xml",  const $ return "<options></options>")
        ]

