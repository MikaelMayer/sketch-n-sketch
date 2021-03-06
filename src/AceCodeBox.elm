port module AceCodeBox exposing
  ( initializeAndDisplay, display, resize, updateFontSize
  , setReadOnly
  , setSelections
  , receiveEditorState
  , userHasTyped
  , resetScroll
  , aceCodeBoxScroll
  )

import Model exposing (Model, AceCodeBoxInfo)
import Ace

--------------------------------------------------------------------------------
-- Ports

-- Outgoing

port aceCodeBoxCmd : AceCodeBoxCmd -> Cmd msg
port aceCodeBoxScroll: { line: Int, col: Int } -> Cmd msg

type alias AceCodeBoxCmd =
  { message : String
  , info : AceCodeBoxInfo
  }

initializeAndDisplay  = sendCmd "initializeAndDisplay"
display               = sendCmd "display"
resize                = sendCmd "resize"
updateFontSize        = sendCmd "updateFontSize"
resetScroll           = sendCmd "resetScroll"

sendCmd message model =
  aceCodeBoxCmd <|
    { message = message
    , info =
        { code = Model.codeToShow model
        , codeBoxInfo = model.codeBoxInfo
        }
    }

port setReadOnly : Bool -> Cmd msg
port setSelections : List Ace.Range -> Cmd msg

-- Incoming

port receiveEditorState : (AceCodeBoxInfo -> msg) -> Sub msg
port userHasTyped : (() -> msg) -> Sub msg
