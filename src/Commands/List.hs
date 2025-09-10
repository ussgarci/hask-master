module Commands.List (
    listP,
    runList,
) where

import Command (Command (..))
import Control.Monad (mapM_)
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Task (Task (..), TaskList (..))

listP :: Parser Command
listP = pure List

runList :: IO ()
runList = do
    home <- getHomeDirectory
    let dataDir = home </> ".hask-master"
    let taskFile = dataDir </> "tasks.json"

    let initialTaskList = TaskList{_tasks = []}

    fileExists <- doesFileExist taskFile
    if fileExists
        then do
            jsonContent <- BS.readFile taskFile
            let taskList = fromMaybe initialTaskList (decode (LBS.fromStrict jsonContent))
            -- putStrLn <$> (_tasks taskList)
            mapM_ print (_tasks taskList)
        else putStrLn "No tasks available."
