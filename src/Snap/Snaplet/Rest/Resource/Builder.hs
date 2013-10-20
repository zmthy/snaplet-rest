{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
module Snap.Snaplet.Rest.Resource.Builder
    ( addMedia
    , setCreate
    , setRead
    , setUpdate
    , setDelete
    , setToDiff
    , setFromParams
    ) where

------------------------------------------------------------------------------
import Control.Monad
import Data.Maybe

------------------------------------------------------------------------------
import Snap.Core (Params)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal
import Snap.Snaplet.Rest.Resource.Media    (Media (..))

import Debug.Trace

tracef :: Show b => (a -> b) -> a -> a
tracef f x = traceShow (f x) x

------------------------------------------------------------------------------
type ResourceBuilder res m id diff
    = Resource res m id diff -> Resource res m id diff


------------------------------------------------------------------------------
addMedia :: Monad m => Media res m diff int -> ResourceBuilder res m id diff
addMedia media res = res
    { renderers   = renderers res ++ renderers'
    , parsers     = parsers res ++ parsers'
    , diffParsers = diffParsers res ++ diffParsers'
    }
  where
    renderers' = fromMaybe [] $ do
        fromResource' <- _fromResource media
        (mts, render) <- responseMedia media
        return $ map (, fromResource' >=> render) mts
    parsers' = fromMaybe [] $ do
        toResource' <- _toResource media
        (mts, parse) <- requestMedia media
        return $ map (, parse >=> maybe (return Nothing) toResource') mts
    diffParsers' = fromMaybe [] $ do
        toDiff' <- _toDiff media
        (mts, parse) <- requestMedia media
        return $ map (, parse >=> maybe (return Nothing) toDiff' . tracef isJust) mts


------------------------------------------------------------------------------
setCreate :: (res -> m ()) -> ResourceBuilder res m id diff
setCreate f res = res { create = Just f }


------------------------------------------------------------------------------
setRead :: (id -> m [res]) -> ResourceBuilder res m id diff
setRead f res = res { retrieve = Just f }


------------------------------------------------------------------------------
setUpdate :: (id -> diff -> m Bool) -> ResourceBuilder res m id diff
setUpdate f res = res { update = Just f }


------------------------------------------------------------------------------
setDelete :: (id -> m Bool) -> ResourceBuilder res m id diff
setDelete f res = res { delete = Just f }


------------------------------------------------------------------------------
setToDiff :: (res -> diff) -> ResourceBuilder res m id diff
setToDiff f res = res { toDiff = Just f }


------------------------------------------------------------------------------
setFromParams :: (Params -> Maybe id) -> ResourceBuilder res m id diff
setFromParams f res = res { fromParams = Just f }

