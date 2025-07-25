module Task (
    Task (..),
) where

import qualified Data.Text as T

data TaskStatus = Todo | InProgress | Paused | Done | Cancelled

data Task = Task
    { _taskId :: Int
    , _taskName :: T.Text
    , _taskStatus :: TaskStatus
    , _taskEstTime :: Int -- estimated time
    }
