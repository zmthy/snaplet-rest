{-# LANGUAGE Rank2Types #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Builder
    (
    -- * Builder
      ResourceBuilder
    , buildResource

    -- * Setters
    , BuildSetter
    , fetch
    , store
    , update
    , delete
    , putAction
    ) where

------------------------------------------------------------------------------
import Data.Maybe
import Control.Lens.Combinators ((<&>))
import Control.Lens.Setter      (Setter)

------------------------------------------------------------------------------
import qualified Snap.Snaplet.Rest.Resource.Internal as Resource

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal
    (Resource (Resource), PutAction (..))


------------------------------------------------------------------------------
data ResourceBuilder rep par m id diff = ResourceBuilder
    { _fetch     :: Maybe (id -> m (Maybe rep))
    , _store     :: Maybe (par -> m ())
    , _update    :: Maybe (id -> diff -> m Bool)
    , _delete    :: Maybe (id -> m Bool)
    , _putAction :: Maybe PutAction
    }


------------------------------------------------------------------------------
buildResource
    :: Functor m => (ResourceBuilder rep par m id diff
    -> ResourceBuilder rep par m id diff)
    -> Resource rep par m id diff
buildResource f = Resource
    { Resource.fetch     = _fetch rb
    , Resource.store     = _store rb
    , Resource.update    = _update rb
    , Resource.delete    = _delete rb
    , Resource.putAction = fromMaybe putDefault $ _putAction rb
    }
  where
    rb = f $ ResourceBuilder Nothing Nothing Nothing Nothing Nothing
    putDefault
        | isJust (_store rb) && isJust (_update rb) = TryUpdate
        | isJust $ _store rb                        = JustStore
        | isJust $ _update rb                       = JustUpdate
        | otherwise                                 = Disabled


------------------------------------------------------------------------------
type BuildSetter rep par m id diff a =
    Setter (ResourceBuilder rep par m id diff)
    (ResourceBuilder rep par m id diff) (Maybe a) a


------------------------------------------------------------------------------
fetch :: BuildSetter rep par m id diff (id -> m (Maybe rep))
fetch f r = f (_fetch r) <&> \a -> r { _fetch = Just a }


------------------------------------------------------------------------------
store :: BuildSetter rep par m id diff (par -> m ())
store f r = f (_store r) <&> \a -> r { _store = Just a }


------------------------------------------------------------------------------
update :: BuildSetter rep par m id diff (id -> diff -> m Bool)
update f r = f (_update r) <&> \a -> r { _update = Just a }


------------------------------------------------------------------------------
delete :: BuildSetter rep par m id diff (id -> m Bool)
delete f r = f (_delete r) <&> \a -> r { _delete = Just a }


------------------------------------------------------------------------------
putAction :: BuildSetter rep par m id diff PutAction
putAction f r = f (_putAction r) <&> \a -> r { _putAction = Just a }

