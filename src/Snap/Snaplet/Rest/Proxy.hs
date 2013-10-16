------------------------------------------------------------------------------
-- | Defines a simple proxy type for complicated type classes.
module Snap.Snaplet.Rest.Proxy
    ( Proxy (..)
    ) where

------------------------------------------------------------------------------
-- | Uses a phantom type to indicate to the type system which class instance
-- to use when it may be ambiguous.
data Proxy t = Proxy

