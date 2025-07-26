module Task (
    Task (..),
) where

import qualified Data.Text as T
import qualified Timer

data TaskStatus = Todo | InProgress | Paused | Done | Cancelled

data Task = Task
    { _name :: T.Text
    , _time :: Int
    , _timer :: Timer.Timer
    }
