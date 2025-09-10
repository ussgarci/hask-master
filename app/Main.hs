{-# LANGUAGE OverloadedStrings #-}

module Main where

import Command (Command (..))
import Commands.Add (runAdd)
import qualified Commands.Add as Add
import Commands.List (runList)
import qualified Commands.List as List
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Task (Task (..), TaskList (..))
import Text.Read (readMaybe)

addP :: Parser Command
addP = Add <$> Add.addP

listP :: Parser Command
listP = List.listP

-- combines sub-commands
commandP :: Parser Command
commandP =
    subparser
        ( command "add" (info addP (progDesc "Add a new task"))
            <> command "list" (info listP (progDesc "List all tasks"))
        )

main :: IO ()
main = do
    cmd <- execParser opts
    run cmd
  where
    opts =
        info
            (commandP <**> helper)
            ( fullDesc
                <> progDesc "A bare-bones task management tool writen in Haskell."
                <> header "hask-master"
            )

run :: Command -> IO ()
run (Add opts) = runAdd opts
run List = runList
