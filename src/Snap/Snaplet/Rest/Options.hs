{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | Specifies client options for given path point on the server.
module Snap.Snaplet.Rest.Options
    ( ResourceOptions
    , optionsFor
    , setAllow
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens.Combinators ((&))
import Data.ByteString          (ByteString)
import Data.Maybe
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Options for a REST resource.
data ResourceOptions = ResourceOptions
    { hasFetch  :: Bool
    , hasStore  :: Bool
    , hasUpdate :: Bool
    , hasDelete :: Bool
    , hasPut    :: Bool
    }


------------------------------------------------------------------------------
-- | Build options for a single resource.
optionsFor :: Resource rep par m id diff -> ResourceOptions
optionsFor res = ResourceOptions
    { hasFetch  = isJust $ fetch res
    , hasStore  = isJust $ store res
    , hasUpdate = isJust $ update res
    , hasDelete = isJust $ delete res
    , hasPut    = case putAction res of
        TryUpdate  -> isJust (store res) && isJust (update res)
        JustStore  -> isJust $ store res
        JustUpdate -> isJust $ update res
    }


------------------------------------------------------------------------------
setAllow :: MonadSnap m => ResourceOptions -> m ()
setAllow opt = modifyResponse . setHeader "Allow" . BS.intercalate "," =<<
    ifTop (return $ collectionAllow opt) <|> (return $ resourceAllow opt)


------------------------------------------------------------------------------
collectionAllow :: ResourceOptions -> [ByteString]
collectionAllow opt = []
    & addMethod (const True) "OPTIONS"
    & addMethod hasStore "POST"
  where addMethod = add opt id


------------------------------------------------------------------------------
resourceAllow :: ResourceOptions -> [ByteString]
resourceAllow opt = []
    & addMethod hasFetch "HEAD"
    & addMethod (const True) "OPTIONS"
    & addMethod hasUpdate "PATCH"
    & addMethod hasDelete "DELETE"
    & addMethod hasPut "PUT"
    & addMethod hasFetch "GET"
  where addMethod = add opt id


------------------------------------------------------------------------------
add :: o -> (a -> b) -> (o -> Bool) -> a -> [b] -> [b]
add opt addf optf verb = if optf opt then (addf verb :) else id

