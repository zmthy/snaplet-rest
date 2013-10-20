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
-- | A resource descriptor for the type 'res'.  The resource runs in the monad
-- 'm', identifies resources with values of the type 'id', and describes
-- changes with value of the type 'diff'.
data Resource res m id diff = Resource
    { renderers     :: [(MediaType, res -> m ByteString)]
    , parsers       :: [(MediaType, ByteString -> m (Maybe res))]
    , diffParsers   :: [(MediaType, ByteString -> m (Maybe diff))]
    , listRenderers :: [(MediaType, [res] -> m ByteString)]
    , listParsers   :: [(MediaType, ByteString -> m (Maybe [res]))]
    , create        :: Maybe (res -> m ())
    , retrieve      :: Maybe (id -> m [res])
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
-- | The empty resource descriptor, useful as a starting point for building
-- resources.
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
    hasCreate  = isJust $ create r
    hasUpdate = isJust $ update r
    defaultPutAction
        | hasCreate && not hasUpdate = Just Create
        | hasUpdate && not hasCreate = Just Update
        | otherwise                 = Nothing


------------------------------------------------------------------------------
-- | Indicates which action that a PUT request should take for a resource.
data PutAction
    = Create  -- ^ Always create
    | Update  -- ^ Always update
    deriving (Eq, Show)

