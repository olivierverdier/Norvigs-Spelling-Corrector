module First where

import Data.Monoid (Monoid(mempty,mappend))
import Data.Maybe (isNothing)

class BoolLike a where
    falsy :: a -> Bool
    bempty :: a

instance BoolLike [a] where
    bempty = []
    falsy = null

newtype First a = MkFirst { getFirst :: a } deriving (Show, Eq)

instance BoolLike a => Monoid (First a) where
    mempty = MkFirst bempty
    mappend (MkFirst l) (MkFirst r) = MkFirst (if falsy l then r else l)


-- For completeness:

instance BoolLike (Maybe a) where
    bempty = Nothing
    falsy = isNothing
