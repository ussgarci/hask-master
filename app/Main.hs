{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Focus (
    focusGetCurrent,
    focusRingCursor,
 )
import Brick.Forms (
    Form,
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
 )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.TH
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Task (Task (..), TaskList (..))
import Text.Read (readMaybe)

instance ToJSON Task
instance FromJSON Task

instance ToJSON TaskList
instance FromJSON TaskList

data Command
    = Add String Int
    | List
    deriving (Show)

addP :: Parser Command
addP =
    Add
        <$> strArgument
            (metavar "<name>" <> help "Name of the task")
        <*> argument
            auto
            (metavar "<minutes>" <> help "Time in minutes for the task")

listP :: Parser Command
listP = pure List

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
run (Add taskName taskMinutes) = do
    home <- getHomeDirectory
    let dataDir = home </> ".hask-master"
    let taskFile = dataDir </> "tasks.json"

    createDirectoryIfMissing True dataDir

    let initialTaskList = TaskList{_tasks = []}

    fileExists <- doesFileExist taskFile
    currentTaskList <-
        if fileExists
            then do
                jsonContent <- LBS.readFile taskFile
                -- return $ maybe initialTaskList id (decode jsonContent)
                return $ Data.Maybe.fromMaybe initialTaskList (decode jsonContent)
            else return initialTaskList

    let newTask = Task{_name = T.pack taskName, _minutes = taskMinutes}
    let updatedTasks = _tasks currentTaskList ++ [newTask]
    let newTaskList = TaskList{_tasks = updatedTasks}

    LBS.writeFile taskFile (encode newTaskList)
    putStrLn $ "Task added: '" ++ taskName ++ "'"
run List =
    putStrLn "NO TASKS YET"
