{-# LANGUAGE ExistentialQuantification, FlexibleInstances, Rank2Types #-}

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
    , fromResourceList
    , toResourceList

    -- * Common instances
    , json
    , jsonFromInstances
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
-- | A grouping of mediatypes and their associated renderers and parsers.  You
-- can use the standard instances defined below, or define your own.
data Media res m diff int = Media
    { _fromResource     :: Maybe (res -> m int)
    , _toResource       :: Maybe (int -> m (Maybe res))
    , _toDiff           :: Maybe (int -> m (Maybe diff))
    , _fromResourceList :: Maybe ([res] -> m int)
    , _toResourceList   :: Maybe (int -> m (Maybe [res]))
    , responseMedia     :: Maybe ([MediaType], int -> m ByteString)
    , requestMedia      :: Maybe ([MediaType], ByteString -> m (Maybe int))
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
-- | Construct a new media grouping with the given response and request
-- mediatypes.
newMedia
    :: (Intermediate int, MonadSnap m) => [MediaType] -> [MediaType]
    -> Media res m diff int
newMedia = newIntermediateMedia defaultFrom defaultTo


------------------------------------------------------------------------------
-- | Construct a new media grouping with response mediatypes only.
newResponseMedia
    :: (int -> m ByteString) -> [MediaType] -> Media res m diff int
newResponseMedia a b =
    Media Nothing Nothing Nothing Nothing Nothing (notEmpty b a) Nothing


------------------------------------------------------------------------------
-- | Construct a new media grouping with request mediatypes only.
newRequestMedia
    :: (ByteString -> m (Maybe int)) -> [MediaType]
    -> Media res m diff int
newRequestMedia a b =
    Media Nothing Nothing Nothing Nothing Nothing Nothing (notEmpty b a)


------------------------------------------------------------------------------
-- | Construct a new media grouping with an intermediate type between the
-- resource and the rendered form.
newIntermediateMedia
    :: (int -> m ByteString) -> (ByteString -> m (Maybe int))
    -> [MediaType] -> [MediaType] -> Media res m diff int
newIntermediateMedia a b x y = Media
    Nothing Nothing Nothing Nothing Nothing (notEmpty x a) (notEmpty y b)


------------------------------------------------------------------------------
notEmpty :: [a] -> f -> Maybe ([a], f)
notEmpty l f = guard (not $ null l) >> Just (l, f)


------------------------------------------------------------------------------
-- | A 'Setter' for defining properties of a media grouping.
type MediaSetter res m diff int f a = Setter
    (Media res m diff int) (Media res m diff int) (f a) a


------------------------------------------------------------------------------
-- | Set the resource renderer.
fromResource :: MediaSetter res m diff int Maybe (res -> m int)
fromResource f m = f (_fromResource m) <&> \g -> m { _fromResource = Just g }


------------------------------------------------------------------------------
-- | Set the resource parser.
toResource :: MediaSetter res m diff int Maybe (int -> m (Maybe res))
toResource f m = f (_toResource m) <&> \g -> m { _toResource = Just g }


------------------------------------------------------------------------------
-- | Set the diff parser.
toDiff :: MediaSetter res m diff int Maybe (int -> m (Maybe diff))
toDiff f m = f (_toDiff m) <&> \g -> m { _toDiff = Just g }


------------------------------------------------------------------------------
-- | Set the resource and diff parser at the same time.
toEither :: MediaSetter res m res int Both (int -> m (Maybe res))
toEither f m = f (_toResource m, _toDiff m) <&> \g -> m
    { _toResource = Just g
    , _toDiff     = Just g
    }

type Both a = (Maybe a, Maybe a)


------------------------------------------------------------------------------
-- | Set the resource list renderer.
fromResourceList :: MediaSetter res m diff int Maybe ([res] -> m int)
fromResourceList f m =
    f (_fromResourceList m) <&> \g -> m { _fromResourceList = Just g }


------------------------------------------------------------------------------
-- | Set the resource list parser.
toResourceList :: MediaSetter res m diff int Maybe (int -> m (Maybe [res]))
toResourceList f m =
    f (_toResourceList m) <&> \g -> m { _toResourceList = Just g }


------------------------------------------------------------------------------
-- | Outputs JSON in UTF-8 and parses JSON agnostic of character set.
json :: Monad m => Media res m diff Value
json = newIntermediateMedia
    (return . LBS.toStrict . encode) (return . decodeStrict)
    ["application/json; charset=utf-8"] ["application/json"]


------------------------------------------------------------------------------
-- | Outputs JSON in UTF-8 and parses JSON agnostic of character set.  Uses
-- the type class instances to automatically set the media methods.
jsonFromInstances
    :: (Monad m, ToJSON res, FromJSON res, FromJSON diff)
    => Media res m diff Value
jsonFromInstances = Media
    (Just (return . toJSON))
    (Just (return . resultToMaybe . fromJSON))
    (Just (return . resultToMaybe . fromJSON))
    (Just (return . toJSON))
    (Just (return . resultToMaybe . fromJSON))
    (Just (["application/json; charset=utf-8"],
        return . LBS.toStrict . encode))
    (Just (["application/json"], return . decode . LBS.fromStrict))


------------------------------------------------------------------------------
resultToMaybe :: Result a -> Maybe a
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

