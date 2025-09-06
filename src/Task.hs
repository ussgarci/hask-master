{-# LANGUAGE DeriveGeneric #-}

module Task (
    Task (..),
    TaskList (..),
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Timer

data TaskStatus = Todo | InProgress | Paused | Done | Cancelled

data Task = Task
    { _name :: T.Text
    , _minutes :: Int
    -- , _timer :: Timer.Timer
    -- , _status :: TaskStatus
    }
    deriving (Show, Generic)

newtype TaskList = TaskList
    { _tasks :: [Task]
    }
    deriving (Show, Generic)
