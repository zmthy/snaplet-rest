{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Internal
    ( Resource (..)
    , resource
    , complete
    , PutAction (..)
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString     (ByteString)
import Data.Maybe
import Network.HTTP.Media  (MediaType)
import Snap.Core           (Params)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.FromRequest.Internal (FromRequest (..))
import Snap.Snaplet.Rest.Proxy                (Proxy (..))


------------------------------------------------------------------------------
data Resource res m id diff = Resource
    { composers     :: [(MediaType, res -> m ByteString)]
    , parsers       :: [(MediaType, ByteString -> m (Maybe res))]
    , diffParsers   :: [(MediaType, ByteString -> m (Maybe diff))]
    , listComposers :: [(MediaType, [res] -> m ByteString)]
    , listParsers   :: [(MediaType, ByteString -> m (Maybe [res]))]
    , fetch         :: Maybe (id -> m [res])
    , store         :: Maybe (res -> m ())
    , update        :: Maybe (id -> diff -> m Bool)
    , toDiff        :: Maybe (res -> diff)
    , delete        :: Maybe (id -> m Bool)
    , fromParams    :: Maybe (Params -> Maybe id)
    , putAction     :: Maybe PutAction
    , patchEnabled  :: Bool
    }


------------------------------------------------------------------------------
class Diff a b where
    defaultToDiff :: Maybe (a -> b)
    isDifferentType :: Proxy (a, b) -> Bool

instance Diff a a where
    defaultToDiff = Just id
    isDifferentType _ = False

instance Diff a b where
    defaultToDiff = Nothing
    isDifferentType _ = True


------------------------------------------------------------------------------
resource :: Resource res m id diff
resource = Resource [] [] [] [] []
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing False


------------------------------------------------------------------------------
complete :: FromRequest id => Resource res m id diff -> Resource res m id diff
complete = complete' Proxy

complete'
    :: FromRequest id
    => Proxy (res, diff) -> Resource res m id diff -> Resource res m id diff
complete' p r = r
    { toDiff       = toDiff r <|> defaultToDiff
    , fromParams   = fromParams r <|> defaultFromParams
    , putAction    = putAction r <|> defaultPutAction
    , patchEnabled = isDifferentType p
    }
  where
    hasStore  = isJust $ store r
    hasUpdate = isJust $ update r
    defaultPutAction
        | hasStore && not hasUpdate = Just Store
        | hasUpdate && not hasStore = Just Update
        | otherwise                 = Nothing


------------------------------------------------------------------------------
-- | Indicates which action that a PUT request should take for a resource.
data PutAction
    = Store   -- ^ Always store
    | Update  -- ^ Always update
    deriving (Eq, Show)

