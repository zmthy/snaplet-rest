{-# LANGUAGE Rank2Types #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Builder
    (
    -- * Builder
      ResourceBuilder
    , buildResource

    -- * Setters
    , exists
    , fetch
    , store
    , update
    , delete
    ) where

------------------------------------------------------------------------------
import Data.Maybe          (isJust)
import Control.Lens.Setter

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal (SplitResource (Resource))


------------------------------------------------------------------------------
data ResourceBuilder rep par m id diff = ResourceBuilder
    { _exists :: Maybe (id -> m Bool)
    , _fetch  :: Maybe (id -> m (Maybe rep))
    , _store  :: Maybe (par -> m ())
    , _update :: Maybe (id -> diff -> m ())
    , _delete :: Maybe (id -> m ())
    }


------------------------------------------------------------------------------
buildResource
    :: (ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id diff)
    -> SplitResource rep par m id diff
buildResource f = Resource
    { Resource.exists = _exists rb
    , Resource.fetch  = _fetch rb
    , Resource.store  = _store rb
    , Resource.update = _update rb
    , Resource.delete = _delete rb
    }
  where
    rb = f $ ResourceBuilder Nothing Nothing Nothing Nothing Nothing


------------------------------------------------------------------------------
type BuildSetter rep par m id diff a =
    Setter (ResourceBuilder rep par m id diff)
    (ResourceBuilder rep par m id diff) (Maybe a) a


------------------------------------------------------------------------------
($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap


------------------------------------------------------------------------------
exists :: BuildSetter rep par m id diff (id -> m Bool)
exists f r = f (_exists r) $> \a -> r { _exists = Just a }


------------------------------------------------------------------------------
fetch :: BuildSetter rep par m id diff (id -> m (Maybe rep))
fetch f r = f (_fetch r) $> \a -> r { _fetch = Just a }


------------------------------------------------------------------------------
store :: BuildSetter rep par m id diff (par -> m ())
store f r = f (_store r) $> \a -> r { _store = Just a }


------------------------------------------------------------------------------
update :: BuildSetter rep par m id diff (id -> diff -> m ())
update f r = f (_update r) $> \a -> r { _update = Just a }


------------------------------------------------------------------------------
delete :: BuildSetter rep par m id diff (id -> m ())
delete f r = f (_delete r) $> \a -> r { _delete = Just a }

