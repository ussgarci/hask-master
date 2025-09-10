module Commands.Add (
    AddOptions (..),
    addP,
    runAdd,
) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Task (Task (..), TaskList (..))

data AddOptions = AddOptions
    { taskName :: String
    , taskMinutes :: Int
    }
    deriving (Show)

addP :: Parser AddOptions
addP =
    AddOptions
        <$> strArgument
            (metavar "<name>" <> help "Name of the task")
        <*> argument
            auto
            (metavar "<minutes>" <> help "Time in minutes for the task")

runAdd :: AddOptions -> IO ()
runAdd opts = do
    home <- getHomeDirectory
    let dataDir = home </> ".hask-master"
    let taskFile = dataDir </> "tasks.json"

    createDirectoryIfMissing True dataDir

    let initialTaskList = TaskList{_tasks = []}

    fileExists <- doesFileExist taskFile
    currentTaskList <-
        if fileExists
            then do
                jsonContent <- BS.readFile taskFile
                return $ fromMaybe initialTaskList (decode (LBS.fromStrict jsonContent))
            else return initialTaskList

    let newTask = Task{_name = T.pack (taskName opts), _minutes = taskMinutes opts}
    let updatedTasks = _tasks currentTaskList ++ [newTask]
    let newTaskList = TaskList{_tasks = updatedTasks}

    BS.writeFile taskFile (LBS.toStrict (encode newTaskList))
    putStrLn $ "Task added: '" ++ taskName opts ++ "'"
