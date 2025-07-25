module Main where

import Byline
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn)

newTask :: MonadByline m => m T.Text
newTask = do
    sayLn $ text (T.pack "Welcome to ") <> (text (T.pack "hask-master") & bold) <> text (T.pack "!")
    let nameQuestion = (text (T.pack "Please enter the task name.") & bold) <> text (T.pack "\n")
    name <- askUntil nameQuestion Nothing (pure . validateName)
    let timeQuestion = (text (T.pack "Please enter the estimated time to completion.") & bold) <> text (T.pack "\n")
    time <- askUntil timeQuestion Nothing (pure . validateTimeEstimate)
    pure $ T.pack "You estimate that " <> name <> T.pack " will take you " <> time <> T.pack "."

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
        Just task -> putStrLn $ T.pack "Task created: " <> task
        _ -> pure ()
