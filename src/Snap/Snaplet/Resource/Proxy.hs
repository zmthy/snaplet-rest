------------------------------------------------------------------------------
-- | A number of resource functions don't have enough information in their
-- types to indicate which resource to operate on.  This module exports the
-- 'Resource' proxy type, which carries the necessary type without actually
-- using it in its value.
module Snap.Snaplet.Resource.Proxy
    ( Resource (..)
    ) where

------------------------------------------------------------------------------
-- | A proxy type to indicate to certain functions what resource to work with
-- if there are no explicit indicators in their type.
--
-- For instance, to serve the Person type as a resource:
--
-- > serve (Resource :: Resource Person)
data Resource r = Resource

