module Timer (
    Timer (..),
    tick,
) where

data Timer = Timer
    { _hours :: Int
    , _minutes :: Int
    , _seconds :: Int
    }
    deriving (Show, Eq, Ord)

tick :: Timer -> Maybe Timer
tick (Timer h m s)
    | s > 0 = Just (Timer h m (s - 1))
    | m > 0 = Just (Timer h (m - 1) 59)
    | h > 0 = Just (Timer (h - 1) 59 59)
    | otherwise = Nothing
