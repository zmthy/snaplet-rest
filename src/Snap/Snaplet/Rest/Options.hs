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
    { hasFetch      :: Bool
    , hasStore      :: Bool
    , hasUpdate     :: Bool
    , hasDiff       :: Bool
    , hasDelete     :: Bool
    , hasFromParams :: Bool
    , hasPut        :: Bool
    }


------------------------------------------------------------------------------
-- | Build options for a single resource.
optionsFor :: Resource res m id diff -> ResourceOptions
optionsFor res = ResourceOptions
    { hasFetch      = isJust $ fetch res
    , hasStore      = isJust $ store res
    , hasUpdate     = isJust $ update res
    , hasDiff       = isJust $ toDiff res
    , hasDelete     = isJust $ delete res
    , hasFromParams = isJust $ fromParams res
    , hasPut        = case putAction res of
        Nothing     -> isJust (store res) && isJust (update res)
        Just Store  -> isJust $ store res
        Just Update -> isJust $ update res
    }


------------------------------------------------------------------------------
setAllow :: MonadSnap m => ResourceOptions -> m ()
setAllow opt =
    ifTop (return (collectionAllow opt))
        <|> return (resourceAllow opt) >>=
    modifyResponse . setHeader "Allow" . BS.intercalate ","


------------------------------------------------------------------------------
collectionAllow :: ResourceOptions -> [ByteString]
collectionAllow opt = []
    & addMethod (hasFetch opt && enabled) "HEAD"
    & addMethod True "OPTIONS"
    & addMethod (hasUpdate opt && enabled) "UPDATE"
    & addMethod (hasDelete opt && enabled) "DELETE"
    & addMethod (hasStore opt && hasDelete opt && enabled) "PUT"
    & addMethod (hasStore opt) "POST"
    & addMethod (hasFetch opt && enabled) "GET"
  where enabled = hasFromParams opt


------------------------------------------------------------------------------
resourceAllow :: ResourceOptions -> [ByteString]
resourceAllow opt = []
    & addMethod (hasFetch opt) "HEAD"
    & addMethod True "OPTIONS"
    & addMethod (hasUpdate opt && hasDiff opt) "PATCH"
    & addMethod (hasDelete opt) "DELETE"
    & addMethod (hasPut opt) "PUT"
    & addMethod (hasFetch opt) "GET"


------------------------------------------------------------------------------
addMethod :: Bool -> ByteString -> [ByteString] -> [ByteString]
addMethod cond verb = if cond then (verb :) else id

