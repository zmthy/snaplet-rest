------------------------------------------------------------------------------
module Snap.Snaplet.Resource.Stored
    ( Stored (..)
    , Info (..)
    , Diff (..)
    ) where

------------------------------------------------------------------------------
import Snap.Core


------------------------------------------------------------------------------
class Stored r where
    retrieve :: MonadSnap m => Info r -> m r
    store    :: MonadSnap m => r -> m ()
    delete   :: MonadSnap m => r -> m ()
    update   :: MonadSnap m => Diff r -> m ()


------------------------------------------------------------------------------
data Info r = Info Int


------------------------------------------------------------------------------
data Diff r = Diff

