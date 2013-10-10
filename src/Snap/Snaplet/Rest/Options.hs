{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | Specifies client options for given path point on the server.
module Snap.Snaplet.Rest.Options
    ( ResourceOptions
    , optionsFor
    , setAllow
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.ByteString     (ByteString)
import Data.Maybe
import Snap.Core

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Media
import Snap.Snaplet.Rest.Resource.Internal


------------------------------------------------------------------------------
-- | Options for a REST resource.
data ResourceOptions = ResourceOptions
    { hasExists :: Bool
    , hasFetch  :: Bool
    , hasStore  :: Bool
    , hasUpdate :: Bool
    , hasDelete :: Bool
    }

instance ToJSON ResourceOptions where
    toJSON opt = toJSON
        $ addOption hasFetch "GET" opt
        $ addOption hasUpdate "PUT" opt
        $ addOption hasUpdate "PATCH" opt
        $ addOption hasDelete "DELETE" opt
        $ addOption hasExists "HEAD" opt
        $ addOption (const True) "OPTIONS" opt
        []
      where addOption = add $ \verb -> object [ "verb" .= String verb ]

instance Monad m => ToMedia ResourceOptions m where
    representations =
        [ ("application/json", return . LBS.toStrict . encode)
        ]


------------------------------------------------------------------------------
-- | Build options for a single resource.
optionsFor :: SplitResource rep par m id diff -> ResourceOptions
optionsFor res = ResourceOptions
    { hasExists = isJust $ exists res
    , hasStore  = isJust $ store res
    , hasFetch  = isJust $ fetch res
    , hasUpdate = isJust $ update res
    , hasDelete = isJust $ delete res
    }


------------------------------------------------------------------------------
setAllow :: MonadSnap m => ResourceOptions -> m ()
setAllow opt = modifyResponse . setHeader "Allow" . BS.intercalate "," =<<
    ifTop (return $ collectionAllow opt) <|> (return $ resourceAllow opt)


------------------------------------------------------------------------------
collectionAllow :: ResourceOptions -> [ByteString]
collectionAllow opt = addMethod hasFetch "POST" opt []
  where addMethod = add id


------------------------------------------------------------------------------
resourceAllow :: ResourceOptions -> [ByteString]
resourceAllow opt =
      addMethod hasFetch "GET" opt
    $ addMethod hasUpdate "PUT" opt
    $ addMethod hasUpdate "PATCH" opt
    $ addMethod hasUpdate "DELETE" opt
    $ addMethod hasUpdate "HEAD" opt
    $ addMethod (const True) "OPTIONS" opt
    []
  where addMethod = add id


------------------------------------------------------------------------------
add :: (a -> b) -> (o -> Bool) -> a -> o -> [b] -> [b]
add addf optf verb opt = if optf opt then (addf verb :) else id

