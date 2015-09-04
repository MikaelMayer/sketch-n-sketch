-- This is the elm file responsible for returning the completed code box given
-- the Model and the appropriate dimensions.

module CodeBox (interpretAceEvents, packageModel, tripRender,
                AceMessage, AceCodeBoxInfo, initAceCodeBoxInfo,
                saveRequestInfo, runRequestInfo) where

import Lang exposing (errorPrefix)

import Graphics.Element as GE
import InterfaceModel as Model exposing (Event, sampleModel, events)

-- So we can check on the installSaveState update
import InterfaceStorage exposing (commitLocalSave)

import Task exposing (Task, andThen)
import String
import Dict exposing (Dict)

-- So we can crash correctly
import Debug

-- We allow a few different types of events to be sent to codeBox.js, including:
-- "assertion"   -> The rest of the contents of the message should supplant the
--                   current corresponding values in the Editor
-- "saveRequest" -> We'd like the current state of the Editor for the purposes
--                   of making a save
-- "runRequest"  -> We'd like the current state of the Editor for the purposes
--                   of running the to be displayed in the Canvas
type alias AceCodeBoxInfo = 
    { kind        : String
    , code        : String 
    , cursorPos   : Model.AcePos
    , manipulable : Bool
    , selections  : List Model.Range
    , highlights  : List Model.Highlight
    , bounce      : Bool
    , exName      : String
    }

type alias AceMessage = 
  { evt          : String 
  , strArg       : String 
  , cursorArg    : Model.AcePos
  , selectionArg : List Model.Range
  , exNameArg    : String
  } 

-- An initial AceCodeBoxInfo for the foldp
-- Doesn't actually get sent over the port
initAceCodeBoxInfo =
  ( { kind = "assertion"
    , code = sampleModel.code
    , cursorPos = sampleModel.codeBoxInfo.cursorPos
    , manipulable = True
    , selections = sampleModel.codeBoxInfo.selections
    , highlights = sampleModel.codeBoxInfo.highlights
    , bounce = True
    , exName = ""
    }
  , []
  )

-- Helper definitons for other messages we can send to Ace
saveRequestInfo : String -> (AceCodeBoxInfo, List Bool)
saveRequestInfo saveName =
  ( { kind = "saveRequest"
    , code = ""
    , cursorPos = sampleModel.codeBoxInfo.cursorPos
    , manipulable = True
    , selections = [] 
    , highlights = []
    , bounce = True
    , exName = saveName
    }
  , []
  )

runRequestInfo : (AceCodeBoxInfo, List Bool)
runRequestInfo =
  ( { kind = "runRequest"
    , code = ""
    , cursorPos = sampleModel.codeBoxInfo.cursorPos
    , manipulable = True
    , selections = [] 
    , highlights = []
    , bounce = True
    , exName = ""
    }
  , []
  )

poke : (AceCodeBoxInfo, List Bool)
poke =
  ( { kind = "poke"
    , code = ""
    , cursorPos = sampleModel.codeBoxInfo.cursorPos
    , manipulable = True
    , selections = [] 
    , highlights = []
    , bounce = True
    , exName = ""
    }
  , []
  )

-- The second model argument is present to allow Saves to be made from here
interpretAceEvents : AceMessage -> Model.Model -> Task String ()
interpretAceEvents amsg model = case Debug.log "From Ace" amsg.evt of
    "runResponse" -> Signal.send events.address <| Model.MultiEvent
      [ Model.UpdateModel <|
            \m -> { m | code <- amsg.strArg
                      , codeBoxInfo <- { cursorPos = amsg.cursorArg
                                       , selections = amsg.selectionArg
                                       , highlights = m.codeBoxInfo.highlights
                                       }
                  }
      , Model.Run
      ]
    --TODO
    "saveResponse" -> 
        let newModel = { model | code <- amsg.strArg
                               , codeBoxInfo <- { cursorPos = amsg.cursorArg
                                                , selections = amsg.selectionArg
                                                , highlights =
                                                    model.codeBoxInfo.highlights
                                                }
                        }
        in commitLocalSave model.exName newModel 
           `andThen` \_ ->
           Signal.send events.address <| Model.UpdateModel <|
                \m -> newModel
    "Rerender" -> Signal.send events.address <| Model.UpdateModel <| \m -> { m | code <- m.code }
    "init" -> Task.succeed () --Signal.send events.address Model.Noop
    _ ->
      -- TODO change this back
      -- if String.contains errorPrefix amsg.evt
      if True
      then Signal.send events.address <| Model.UpdateModel <| recoverFromError amsg
      -- TODO: this leads to an infinite loop of restarting in Chrome...
      -- else Debug.crash "Malformed update sent to Elm"
      else Signal.send events.address Model.Noop

-- Puts us in the correct state if we recovered from an error, which we find out
-- about from the JS that also happens to load Ace.
-- Maybe we should split this out into a different Elm/JS file?
recoverFromError : AceMessage -> Model.Model -> Model.Model
recoverFromError amsg fresh = 
    { fresh | code <- amsg.strArg
            , editingMode <- Just amsg.strArg
            , errorBox <- Just amsg.evt
            , exName <- amsg.exNameArg
            , codeBoxInfo <- { selections = amsg.selectionArg
                             , cursorPos  = amsg.cursorArg
                             , highlights = fresh.codeBoxInfo.highlights
                             }
    }

-- The number of times that we defensively rerender the codebox on codebox
-- clobbering updates. Determined experimentally.
-- We shouldn't have to do this. For some reason the Elm runtime will rerender
-- parts of the page *after* sending signals out to ports.
-- Note that each one of these won't necessarily trigger a DOM copy/replacement;
-- it only will for each of the times that Elm clobbers it.
rerenderCount : Int
rerenderCount = 6

packageModel : (Model.Model, Event) -> (AceCodeBoxInfo, List Bool) -> 
                    (AceCodeBoxInfo, List Bool)
packageModel (model, evt) (lastBox, rerenders) = 
    let manipulable = case (model.mode, model.editingMode) of
            (Model.SaveDialog _, _) -> False
            (_, Nothing) -> False
            _           -> True
        rerender = tripRender evt rerenders
    in case evt of
      Model.WaitSave saveName -> saveRequestInfo saveName
      Model.WaitRun  -> runRequestInfo
      Model.KeysDown _ -> poke
      _ ->
          ( { kind = "assertion"
            , code = model.code 
            , cursorPos = model.codeBoxInfo.cursorPos 
            , selections = model.codeBoxInfo.selections
            , manipulable = manipulable
            , highlights = model.codeBoxInfo.highlights
            , bounce = rerender
            , exName = model.exName
            }
          , rerender :: List.take (rerenderCount - 1) rerenders
          )

-- Lets a signal pass if it should triger an extra rerender
-- This is entered into a foldp so that we do not enter into an infinite
-- rerender loop. Currently, all button presses are separated by at least a
-- MouseDown event, meaning that we should never miss a rerender.
-- We have the Bool list because, experimentally, one rerender is not
-- 'enough'. Occasionally the rerender still gets clobbered. Ugh.
-- Note that the initial list population is important for fixing the blanking on
-- the page load.
tripRender : Event -> List Bool -> Bool
tripRender evt last = 
  if List.all (\a -> a) last then False else
    case (evt, last) of
      (_                 , True :: rest  ) -> True
      (Model.SwitchOrient, _             ) -> True
      (Model.InstallSaveState, _         ) -> True
      (Model.RemoveDialog _ _ , _        ) -> True
      (Model.SetBasicCodeBox _ , _       ) -> True
      _                                    -> False
