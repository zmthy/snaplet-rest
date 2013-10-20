{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
    FunctionalDependencies, Rank2Types #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Media
    (
    -- * Type
      Media (..)
    , newMedia
    , newResponseMedia
    , newRequestMedia
    , newIntermediateMedia

    -- * Setters
    , MediaSetter
    , fromResource
    , toResource
    , toDiff
    , toEither

    -- * Common instances
    , json
    , jsonInstances
    , xml
    , xhtml
    , html
    , form
    , multipart
    ) where

------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.UTF8     as BS
import qualified Text.XmlHtml             as Xml

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad
import Data.Aeson         hiding (json)
import Data.ByteString    (ByteString)
import Network.HTTP.Media (MediaType)
import Snap.Core
import Text.XmlHtml       (Document)


------------------------------------------------------------------------------
data Media res m diff int = Media
    { _fromResource    :: Maybe (res -> m int)
    , _toResource      :: Maybe (int -> m (Maybe res))
    , _toDiff          :: Maybe (int -> m (Maybe diff))
    , responseMedia    :: Maybe ([MediaType], int -> m ByteString)
    , requestMedia     :: Maybe ([MediaType], ByteString -> m (Maybe int))
    }


------------------------------------------------------------------------------
-- | Convenience class that allows 'serialize' and 'parse' to be implemented
-- with a default for some types.
class Intermediate int where
    defaultFrom :: MonadSnap m => int -> m ByteString
    defaultTo   :: MonadSnap m => ByteString -> m (Maybe int)

instance Intermediate ByteString where
    defaultFrom = return
    defaultTo   = return . Just

instance Intermediate String where
    defaultFrom = return . BS.fromString
    defaultTo   = return . Just . BS.toString


------------------------------------------------------------------------------
newMedia
    :: (Intermediate int, MonadSnap m) => [MediaType] -> [MediaType]
    -> Media res m diff int
newMedia = newIntermediateMedia defaultFrom defaultTo


------------------------------------------------------------------------------
newResponseMedia
    :: (int -> m ByteString) -> [MediaType] -> Media res m diff int
newResponseMedia a b = Media Nothing Nothing Nothing (notEmpty b a) Nothing


------------------------------------------------------------------------------
newRequestMedia
    :: (ByteString -> m (Maybe int)) -> [MediaType]
    -> Media res m diff int
newRequestMedia a b = Media Nothing Nothing Nothing Nothing (notEmpty b a)


------------------------------------------------------------------------------
newIntermediateMedia
    :: (int -> m ByteString) -> (ByteString -> m (Maybe int))
    -> [MediaType] -> [MediaType] -> Media res m diff int
newIntermediateMedia a b x y =
    Media Nothing Nothing Nothing (notEmpty x a) (notEmpty y b)


------------------------------------------------------------------------------
notEmpty :: [a] -> f -> Maybe ([a], f)
notEmpty l f = guard (not $ null l) >> Just (l, f)


------------------------------------------------------------------------------
type MediaSetter res m diff int f a = Setter
    (Media res m diff int) (Media res m diff int) (f a) a


------------------------------------------------------------------------------
fromResource :: MediaSetter res m diff int Maybe (res -> m int)
fromResource f m = f (_fromResource m) <&> \g -> m { _fromResource = Just g }


------------------------------------------------------------------------------
toResource :: MediaSetter res m diff int Maybe (int -> m (Maybe res))
toResource f m = f (_toResource m) <&> \g -> m { _toResource = Just g }


------------------------------------------------------------------------------
toDiff :: MediaSetter res m diff int Maybe (int -> m (Maybe diff))
toDiff f m = f (_toDiff m) <&> \g -> m { _toDiff = Just g }


------------------------------------------------------------------------------
toEither :: MediaSetter res m res int Both (int -> m (Maybe res))
toEither f m = f (_toResource m, _toDiff m) <&> \g -> m
    { _toResource = Just g
    , _toDiff     = Just g
    }

type Both a = (Maybe a, Maybe a)


------------------------------------------------------------------------------
-- | Outputs JSON in UTF-8 and parses JSON agnostic of character set.
json :: Monad m => Media res m diff Value
json = newIntermediateMedia
    (return . LBS.toStrict . encode) (return . decodeStrict)
    ["application/json; charset=utf-8"] ["application/json"]


------------------------------------------------------------------------------
-- | Outputs JSON in UTF-8 and parses JSON agnostic of character set.  Uses
-- the type class instances to automatically set the media methods.
jsonInstances
    :: (Monad m, ToJSON res, FromJSON res, FromJSON diff)
    => Media res m diff Value
jsonInstances = Media
    (Just (return . toJSON))
    (Just (return . resultToMaybe . fromJSON))
    (Just (return . resultToMaybe . fromJSON))
    (Just (["application/json; charset=utf-8"],
        return . LBS.toStrict . encode))
    (Just (["application/json"], return . decode . LBS.fromStrict))
  where
    resultToMaybe (Error _)   = Nothing
    resultToMaybe (Success a) = Just a


------------------------------------------------------------------------------
-- | Outputs XML in UTF-8 and parses XML agnostic of character set.
xml :: Monad m => Media res m diff Document
xml = newIntermediateMedia
    (return . BB.toByteString . Xml.render)
    (return . either (const Nothing) Just . Xml.parseXML "")
    ["application/xml; charset=utf-8"] ["application/xml"]


------------------------------------------------------------------------------
-- | Supports both XHTML and HTML in UTF-8 as the output format only.
-- Recommended over 'html' if the output will be valid XHTML.
xhtml :: MonadSnap m => Media res m diff ByteString
xhtml = newMedia
    ["application/xhtml+xml; charset=utf-8", "text/html; charset=utf-8"] []


------------------------------------------------------------------------------
-- | Supports HTML in UTF-8 as the output format only.  Use 'xhtml' if the
-- output is guaranteed to be well formed.
html :: MonadSnap m => Media res m diff ByteString
html = newMedia ["text/html; charset=utf-8"] []


------------------------------------------------------------------------------
-- | Supports URL-encoded web forms as the input format only.
form :: MonadSnap m => Media res m diff Params
form = newRequestMedia (const $ fmap Just getParams)
    ["application/x-www-form-urlencoded"]


------------------------------------------------------------------------------
-- | Supports multipart web forms as the input format only.
multipart :: MonadSnap m => Media res m diff ByteString
multipart = newMedia [] ["multipart/form-data"]

