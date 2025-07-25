module Main where

import Byline
import Control.Concurrent (threadDelay)
import Data.Function ((&))
import qualified Data.Maybe
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import System.IO (hFlush, stdout)
import Task (Task (..))
import Text.Read (readMaybe)
import Prelude hiding (putStrLn)

countdown :: Int -> IO ()
countdown 0 = putStrLn $ T.pack "\nTime's up!"
countdown n = do
    putStr (show n ++ " seconds remaining...\r")
    hFlush stdout
    threadDelay 1000000
    countdown (n - 1)

newTask :: MonadByline m => m Task
newTask = do
    sayLn $ text (T.pack "Welcome to ") <> (text (T.pack "hask-master") & bold) <> text (T.pack "!")
    let nameQuestion = (text (T.pack "Please enter the task name.") & bold) <> text (T.pack "\n")
    name <- askUntil nameQuestion Nothing (pure . validateName)
    let timeQuestion = (text (T.pack "Please enter the estimated time to completion.") & bold) <> text (T.pack "\n")
    timeStr <- askUntil timeQuestion Nothing (pure . validateTimeEstimate)
    let timeInt = Data.Maybe.fromMaybe 0 (readMaybe (T.unpack timeStr))
    pure $ Task{_taskName = name, _taskEstTime = timeInt}

validateName :: T.Text -> Either (Stylized T.Text) T.Text
validateName input
    | T.null input = Left $ text (T.pack "A task name is required.") & bg red
    | otherwise = Right input

validateTimeEstimate :: T.Text -> Either (Stylized T.Text) T.Text
validateTimeEstimate input
    | T.length input < 1 = Left $ text (T.pack "Estimated time must be a positive integer.") & bg red
    | otherwise = Right input

main :: IO ()
main = do
    result <- runBylineT newTask
    case result of
        Just task -> do
            countdown $ _taskEstTime task
        _ -> pure ()
