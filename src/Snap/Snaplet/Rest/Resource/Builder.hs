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
    , setPutAction
    ) where

------------------------------------------------------------------------------
import Control.Monad
import Data.Maybe

------------------------------------------------------------------------------
import Snap.Core (Params)

------------------------------------------------------------------------------
import Snap.Snaplet.Rest.Resource.Internal
import Snap.Snaplet.Rest.Resource.Media    (Media (..))


------------------------------------------------------------------------------
type ResourceBuilder res m id diff
    = Resource res m id diff -> Resource res m id diff


------------------------------------------------------------------------------
-- | Add a media representation for rendering and parsing.
addMedia :: Monad m => Media res m diff int -> ResourceBuilder res m id diff
addMedia media res = res
    { renderers     = renderers res ++ renderers'
    , parsers       = parsers res ++ parsers'
    , diffParsers   = diffParsers res ++ diffParsers'
    , listRenderers = listRenderers res ++ listRenderers'
    , listParsers   = listParsers res ++ listParsers'
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
        return $ map (, parse >=> maybe (return Nothing) toDiff') mts
    listRenderers' = fromMaybe [] $ do
        fromResourceList' <- _fromResourceList media
        (mts, render) <- responseMedia media
        return $ map (, fromResourceList' >=> render) mts
    listParsers' = fromMaybe [] $ do
        toResourceList' <- _toResourceList media
        (mts, parse) <- requestMedia media
        return $ map (, parse >=> maybe (return Nothing) toResourceList') mts


------------------------------------------------------------------------------
-- | Set the create method for the resource.
setCreate :: (res -> m ()) -> ResourceBuilder res m id diff
setCreate f res = res { create = Just f }


------------------------------------------------------------------------------
-- | Set the read method for the resource.
setRead :: (id -> m [res]) -> ResourceBuilder res m id diff
setRead f res = res { retrieve = Just f }


------------------------------------------------------------------------------
-- | Set the update method for the resource.  The method must return
-- a boolean, indicating whether anything was updated.
setUpdate :: (id -> diff -> m Bool) -> ResourceBuilder res m id diff
setUpdate f res = res { update = Just f }


------------------------------------------------------------------------------
-- | Set the delete method for the resource.  The method must return a
-- boolean, indicating whether anything was deleted.
setDelete :: (id -> m Bool) -> ResourceBuilder res m id diff
setDelete f res = res { delete = Just f }


------------------------------------------------------------------------------
-- | Sets the conversion function from resource to diff value.
setToDiff :: (res -> diff) -> ResourceBuilder res m id diff
setToDiff f res = res { toDiff = Just f }


------------------------------------------------------------------------------
-- | Sets the URL query string parser.
setFromParams :: (Params -> Maybe id) -> ResourceBuilder res m id diff
setFromParams f res = res { fromParams = Just f }


------------------------------------------------------------------------------
-- | Sets a specific action to take when a PUT method is received.  If not
-- set, this defaults to trying to update and then creating if that fails.
setPutAction :: PutAction -> ResourceBuilder res m id diff
setPutAction a res = res { putAction = Just a }

