{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | Specifies client options for given path point on the server.
module Snap.Snaplet.Rest.Options
    ( ResourceOptions
    , optionsFor
    , setAllow
    , resourceAllow
    , collectionAllow
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens.Combinators ((&))
import Data.Maybe
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Options for a REST resource.
data ResourceOptions = ResourceOptions
    { hasRetrieve      :: Bool
    , hasCreate        :: Bool
    , hasUpdate        :: Bool
    , hasDiff          :: Bool
    , hasDelete        :: Bool
    , hasListRenderers :: Bool
    , hasListParsers   :: Bool
    , hasFromParams    :: Bool
    , hasPut           :: Bool
    }


------------------------------------------------------------------------------
-- | Build options for a single resource.
optionsFor :: Resource res m id diff -> ResourceOptions
optionsFor res = ResourceOptions
    { hasRetrieve      = isJust $ retrieve res
    , hasCreate        = isJust $ create res
    , hasUpdate        = isJust $ update res
    , hasDiff          = isJust $ toDiff res
    , hasDelete        = isJust $ delete res
    , hasListRenderers = not . null $ listRenderers res
    , hasListParsers   = not . null $ listParsers res
    , hasFromParams    = isJust $ fromParams res
    , hasPut           = case putAction res of
        Nothing     -> isJust (create res) && isJust (update res)
        Just Create -> isJust $ create res
        Just Update -> isJust $ update res
    }


------------------------------------------------------------------------------
setAllow :: MonadSnap m => ResourceOptions -> m ()
setAllow opt =
    ifTop (return (collectionAllow opt))
        <|> return (resourceAllow opt) >>=
    modifyResponse . setHeader "Allow" . BS.intercalate "," .
        map (BS.fromString . show)


------------------------------------------------------------------------------
collectionAllow :: ResourceOptions -> [Method]
collectionAllow opt = []
    & addMethod (hasRetrieve opt && readEnabled) HEAD
    & addMethod True OPTIONS
    & addMethod (hasUpdate opt && writeEnabled) PATCH
    & addMethod (hasDelete opt && writeEnabled) DELETE
    & addMethod (hasCreate opt && hasDelete opt && writeEnabled) PUT
    & addMethod (hasCreate opt) POST
    & addMethod (hasRetrieve opt && readEnabled) GET
  where
    readEnabled = hasFromParams opt && hasListRenderers opt
    writeEnabled = hasFromParams opt && hasListParsers opt


------------------------------------------------------------------------------
resourceAllow :: ResourceOptions -> [Method]
resourceAllow opt = []
    & addMethod (hasRetrieve opt) HEAD
    & addMethod True OPTIONS
    & addMethod (hasUpdate opt && hasDiff opt) PATCH
    & addMethod (hasDelete opt) DELETE
    & addMethod (hasPut opt) PUT
    & addMethod (hasRetrieve opt) GET


------------------------------------------------------------------------------
addMethod :: Bool -> Method -> [Method] -> [Method]
addMethod cond verb = if cond then (verb :) else id

