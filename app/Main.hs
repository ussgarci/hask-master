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
import Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Task (Task (..))
import Text.Read (readMaybe)
import Timer (Timer (..))
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

data TimerState = Running | Paused

data AppState = AppState
    { _tasks :: [Task]
    , _taskMap :: Map.Map String (ThreadId, MVar TimerState)
    }

data TaskFormFields = NameField
                    | TimeField
                    deriving (Eq, Ord, Show)

makeLenses ''Task

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: Task -> Form Task e TaskFormFields
mkForm =
    let label s w = padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Name" @@=
                   editTextField name NameField (Just 1)
               , label "Age" @@=
                   editShowableField time TimeField
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form Task e TaskFormFields -> [Widget TaskFormFields]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Time must be a positive integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form Task e TaskFormFields) e TaskFormFields
app =
    App { appDraw = draw
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent (V.EvKey V.KEsc []) -> halt
                -- Enter quits only when we aren't in the multi-line editor.
                -- VtyEvent (V.EvKey V.KEnter [])
                --     | focusGetCurrent f /= Just DescriptionField -> halt
                _ -> do
                    handleFormEvent ev

                    -- Example of external validation:
                    -- Require time field to be a positive integer.
                    st <- gets formState
                    modify $ setFieldValid (st^.time >= 1) TimeField

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let buildVty = do
          v <- mkVty V.defaultConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialTaskInfo = Task { _name = T.pack ""
                               , _time = 0
                               , _timer = Timer { _hours = 0
                                                 , _minutes = 0
                                                 , _seconds = 0
                                                }
                               }
        f = setFieldValid False TimeField $
            mkForm initialTaskInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    TIO.putStrLn $ T.pack "The starting form state was:"
    -- print initialTaskInfo
    print "create a show instance for Task"

    TIO.putStrLn $ T.pack "The final form state was:"
    -- print $ formState f'
    print "create a show instance for Task"

    if allFieldsValid f'
       then TIO.putStrLn $ T.pack "The final form inputs were valid."
       else TIO.putStrLn $ T.pack $ "The final form had invalid inputs: " <> show (invalidFields f')
