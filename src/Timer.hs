module Timer (
    Timer (..),
) where

data Timer = Timer
    { hours :: Int
    , minutes :: Int
    , seconds :: Int
    }
    deriving (Show, Eq, Ord)
