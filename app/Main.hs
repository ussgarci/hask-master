{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Lens.Micro ((^.))
import Lens.Micro.TH
import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.MVar (MVar)
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Task (Task (..))
import Text.Read (readMaybe)
import Timer (Timer (..))
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Options.Applicative

data Command
  = Add String Int
  | List
  deriving (Show)

addP :: Parser Command
addP = Add
  <$> strArgument
      ( metavar "<name>" <> help "Name of the task" )
  <*> argument auto
      ( metavar "<minutes>" <> help "Time in minutes for the task" )

listP :: Parser Command
listP = pure List

-- combines sub-commands
commandP :: Parser Command
commandP = subparser
  ( command "add" (info addP (progDesc "Add a new task"))
 <> command "list" (info listP (progDesc "List all tasks"))
  )

main :: IO ()
main = do
  cmd <- execParser opts
  run cmd
  where
    opts = info (commandP <**> helper)
      ( fullDesc
     <> progDesc "A bare-bones task management tool writen in Haskell."
     <> header "hask-master"
      )

run :: Command -> IO ()
run (Add name minutes) =
  putStrLn $ "Adding task '" ++ name ++ "' which takes " ++ show minutes ++ " minutes."
run List =
  putStrLn "NO TASKS YET"


