{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Path
    ( FromPath (..)
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


------------------------------------------------------------------------------
-- | Instances of 'FromPath' can be produced from part of a URL path.
class FromPath i where
    fromPath :: ByteString -> Maybe i

instance FromPath Int where
    fromPath p = safeRead . reads $ BS.toString p
      where
        safeRead [(i, "")] = Just i
        safeRead _         = Nothing

instance FromPath (CI String) where
    fromPath p = mk . BS.toString <$> notEmpty p

instance FromPath (CI Text.Text) where
    fromPath p = mk . Text.pack . BS.toString <$> notEmpty p

instance FromPath (CI ByteString) where
    fromPath p = mk <$> notEmpty p

instance FromPath (CI LBS.ByteString) where
    fromPath p = mk . LBS.fromStrict <$> notEmpty p

instance FromPath a => FromPath [a] where
    fromPath = mapM (notEmpty >=> fromPath) . BS.split 47

instance (FromPath a, FromPath b) => FromPath (a, b) where
    fromPath p = do
        let (a, b) = BS.breakByte 47 p
        a' <- fromPath a
        b' <- fromPath b
        return (a', b')

instance (FromPath a, FromPath b, FromPath c) => FromPath (a, b, c) where
    fromPath p = do
        let (a, r) = BS.breakByte 47 p
        a' <- fromPath a
        (b, c) <- fromPath r
        return (a', b, c)


------------------------------------------------------------------------------
-- | Ensures that the given 'ByteString' is not empty, evaluating to 'Nothing'
-- if it is.
notEmpty :: ByteString -> Maybe ByteString
notEmpty bs = if BS.null bs then Nothing else Just bs

