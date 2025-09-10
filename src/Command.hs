module Command (Command (..)) where

import Commands.Add (AddOptions)

data Command
    = Add AddOptions
    | List
    deriving (Show)
