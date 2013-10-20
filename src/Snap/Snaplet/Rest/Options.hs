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
    { hasRetrieve   :: Bool
    , hasCreate     :: Bool
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
    { hasRetrieve   = isJust $ retrieve res
    , hasCreate     = isJust $ create res
    , hasUpdate     = isJust $ update res
    , hasDiff       = isJust $ toDiff res
    , hasDelete     = isJust $ delete res
    , hasFromParams = isJust $ fromParams res
    , hasPut        = case putAction res of
        Nothing     -> isJust (create res) && isJust (update res)
        Just Create -> isJust $ create res
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
    & addMethod (hasRetrieve opt && enabled) "HEAD"
    & addMethod True "OPTIONS"
    & addMethod (hasUpdate opt && enabled) "UPDATE"
    & addMethod (hasDelete opt && enabled) "DELETE"
    & addMethod (hasCreate opt && hasDelete opt && enabled) "PUT"
    & addMethod (hasCreate opt) "POST"
    & addMethod (hasRetrieve opt && enabled) "GET"
  where enabled = hasFromParams opt


------------------------------------------------------------------------------
resourceAllow :: ResourceOptions -> [ByteString]
resourceAllow opt = []
    & addMethod (hasRetrieve opt) "HEAD"
    & addMethod True "OPTIONS"
    & addMethod (hasUpdate opt && hasDiff opt) "PATCH"
    & addMethod (hasDelete opt) "DELETE"
    & addMethod (hasPut opt) "PUT"
    & addMethod (hasRetrieve opt) "GET"


------------------------------------------------------------------------------
addMethod :: Bool -> ByteString -> [ByteString] -> [ByteString]
addMethod cond verb = if cond then (verb :) else id

