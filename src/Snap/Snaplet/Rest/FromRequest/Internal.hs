{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.FromRequest.Internal
    ( FromRequest (..)
    , parseRead
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text            as Text

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI, mk)
import Data.Void            (Void)
import Snap.Core            (Params)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Proxy (Proxy)


------------------------------------------------------------------------------
-- | Instances of this class can be parsed from the remaining path information
-- at the current route, and potentially also the URL parameters.
class FromRequest id where

    -- | Parse a value from the remaining path information.  A value of
    -- 'Nothing' indicates that the parse failed.
    fromPath :: ByteString -> Maybe id

    -- | Internal method that indicates if individual resources are disabled.
    pathEnabled :: Proxy id -> Bool
    pathEnabled _ = True

    -- | Internal method that allows standard types to provide a default
    -- implementation of 'fromParams'.
    defaultFromParams :: Maybe (Params -> Maybe id)
    defaultFromParams = Nothing

-- This instance is useful for a resource where POST is the only available
-- method with a side-effect.  Indicates that the resource cannot be
-- identified or searched.
instance FromRequest Void where
    fromPath _ = Nothing
    pathEnabled _ = False

-- This instance is useful for a singleton resource.  Indicates that the
-- resource can only be accessed at its root, and cannot be searched.
instance FromRequest () where
    fromPath _ = Nothing
    pathEnabled _ = False

-- This instance can be used to indicate that while query string searching is
-- disabled, operations can still be performed on collections.
instance FromRequest a => FromRequest (Maybe a) where
    fromPath = Just . fromPath
    defaultFromParams = Just $ const Nothing

-- Useful for disparate path and query types.
instance FromRequest a => FromRequest (Either a b) where
    fromPath = fmap Left . fromPath

instance FromRequest Int where
    fromPath = parseRead

instance FromRequest (CI String) where
    fromPath p = mk . BS.toString <$> checkSplit p

instance FromRequest (CI Text.Text) where
    fromPath p = mk . Text.pack . BS.toString <$> checkSplit p

instance FromRequest (CI ByteString) where
    fromPath p = mk <$> checkSplit p

instance FromRequest (CI LBS.ByteString) where
    fromPath p = mk . LBS.fromStrict <$> checkSplit p

instance FromRequest a => FromRequest [a] where
    fromPath = mapM (checkSplit >=> fromPath) . BS.split 47

instance (FromRequest a, FromRequest b) => FromRequest (a, b) where
    fromPath p = do
        let (a, b) = BS.breakByte 47 p
        a' <- fromPath a
        b' <- fromPath b
        return (a', b')

instance (FromRequest a, FromRequest b, FromRequest c)
        => FromRequest (a, b, c) where
    fromPath p = do
        let (a, r) = BS.breakByte 47 p
        a' <- fromPath a
        (b, c) <- fromPath r
        return (a', b, c)

instance (FromRequest a, FromRequest b, FromRequest c, FromRequest d)
        => FromRequest (a, b, c, d) where
    fromPath p = do
        let (a, r) = BS.breakByte 47 p
        a' <- fromPath a
        (b, c, d) <- fromPath r
        return (a', b, c, d)


------------------------------------------------------------------------------
-- | Ensures that the given 'ByteString' is neither empty nor containing a
-- path split, evaluating to 'Nothing' in either case.
checkSplit :: ByteString -> Maybe ByteString
checkSplit bs = if length (BS.split 47 bs) /= 1 then Nothing else Just bs


------------------------------------------------------------------------------
-- | A convenient helper function that wraps a read failure into 'Nothing'
-- instead of throwing an error.
parseRead :: Read a => ByteString -> Maybe a
parseRead = safeRead . reads . BS.toString
  where
    safeRead [(a, "")] = Just a
    safeRead _         = Nothing

