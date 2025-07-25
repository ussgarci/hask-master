module Task (
    Task (..),
) where

import qualified Data.Text as T

data TaskStatus = Todo | InProgress | Paused | Done | Cancelled

data Task = Task
    { _taskName :: T.Text
    , _taskEstTime :: Int
    }
