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
import Snap.Snaplet.Rest.Diff.Internal     (Diff, patchDisabled)
import Snap.Snaplet.Rest.Resource.Internal
import Snap.Snaplet.Rest.Proxy             (Proxy)


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
setAllow
    :: (MonadSnap m, Diff par diff)
    => Proxy (par, diff) -> ResourceOptions -> m ()
setAllow p opt = modifyResponse . setHeader "Allow" . BS.intercalate "," =<<
    ifTop (return $ collectionAllow opt) <|> (return $ resourceAllow p opt)


------------------------------------------------------------------------------
collectionAllow :: ResourceOptions -> [ByteString]
collectionAllow opt = []
    & addMethod True "OPTIONS"
    & addMethod (hasStore opt) "POST"
    & addMethod (hasFetch opt) "GET"


------------------------------------------------------------------------------
resourceAllow
    :: Diff par diff => Proxy (par, diff) -> ResourceOptions -> [ByteString]
resourceAllow p opt = []
    & addMethod (hasFetch opt) "HEAD"
    & addMethod True "OPTIONS"
    & addMethod (hasUpdate opt && not (patchDisabled p)) "PATCH"
    & addMethod (hasDelete opt) "DELETE"
    & addMethod (hasPut opt) "PUT"
    & addMethod (hasFetch opt) "GET"


------------------------------------------------------------------------------
addMethod :: Bool -> ByteString -> [ByteString] -> [ByteString]
addMethod cond verb = if cond then (verb :) else id

