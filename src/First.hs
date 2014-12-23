module First where

import Data.Monoid (Monoid(mempty,mappend))
import Data.Maybe (isNothing)

class BoolLike a where
    falthy :: a -> Bool
    bempty :: a

instance BoolLike [a] where
    bempty = []
    falthy = null

newtype First a = First { getFirst :: a } deriving (Show, Eq)

instance BoolLike a => Monoid (First a) where
    mempty = First bempty
    mappend (First l) (First r) = First $ if falthy l then r else l


-- For completeness:

instance BoolLike (Maybe a) where
    bempty = Nothing
    falthy = isNothing
