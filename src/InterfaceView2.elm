module InterfaceView2 (view) where

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 as Parser exposing (parseE, parseV)
import Sync
import Eval
import Utils
import MicroTests
import InterfaceModel exposing (..)
import LangHtml exposing (toNum, toNumTr, addi)
import ExamplesGenerated as Examples
import Config exposing (params)
import OurParser2 as P

import VirtualDom

--Core Libraries
import List 
import Dict
import Set
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC
import Graphics.Input as GI
import Graphics.Input.Field as GIF
import Text as T exposing (defaultStyle)
import Color

--Signaling Libraries
import Mouse 
import Window 
import Task exposing (Task, andThen)

--Storage Libraries
import InterfaceStorage exposing (taskMailbox, saveStateLocally, loadLocalState,
                                  checkAndSave, getLocalSaves, clearLocalSaves,
                                  deleteLocalSave)

--Html Libraries
import Html 
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

--Error Checking Libraries
import Debug


--------------------------------------------------------------------------------

dimToPix d = String.append (toString d) "px"

interfaceColor = Color.rgba 52 73 94 1.0
textColor = "white"

titleStyle =
  { defaultStyle | typeface <- ["Courier", "monospace"]
                 , height <- Just 18
                 , bold <- False 
                 , color <- Color.white}

-- Creates an Html button with the text properly offset
type ButtonStatus = Raised | Highlighted | Depressed | Disabled

-- Currently assumes:
--  font-size is 16px
--  the top of the button occupies 90% of the height of the button
--  the depressed button should move the text down 3/50 of the total height of the
--   button
makeButton : ButtonStatus -> Int -> Int -> String -> GE.Element
makeButton status w h text =
  let fontsize = 16
      topprop = 0.9
      depdip = 0.06
      raisedoffset = round <| 0.5 * topprop * toFloat h - 0.5 * fontsize
      depressedoffset = round <| toFloat raisedoffset + depdip * toFloat h
      (img,dip) = case status of
    Raised      -> ("button_raised.svg", dimToPix raisedoffset)
    Highlighted -> ("button_highlighted.svg", dimToPix raisedoffset)
    Depressed   -> ("button_depressed.svg", dimToPix depressedoffset)
    Disabled    -> ("button_disabled.svg", dimToPix raisedoffset)
  in
  GE.flow GE.outward
    [ GE.image w h img
    , Html.toElement w h <|
        Html.div
          [ Attr.style
              [ ("color", textColor)
              , ("font-family", "sans-serif")
              , ("text-align", "center")
              , ("width", dimToPix w)
              , ("height", dimToPix h)
              , ("transform", "translate(0px," ++ dip ++ ")")
              ]
          ] [ Html.text text ]
    ]

--------------------------------------------------------------------------------
-- Zone Options (per shape)

type alias ZoneOptions =
  { showBasic : Bool , addBasic : Bool , addRot : Bool , addColor : Bool }

zoneOptions0 =
  { showBasic = False , addBasic = False , addRot = False , addColor = False }

optionsOf : ShowZones -> ZoneOptions
optionsOf x =
  if | x == showZonesNone  -> { zoneOptions0 | addBasic <- True }
     | x == showZonesBasic -> { zoneOptions0 | addBasic <- True, showBasic <- True }
     | x == showZonesRot   -> { zoneOptions0 | addRot <- True }
     | x == showZonesColor -> { zoneOptions0 | addColor <- True }

--------------------------------------------------------------------------------
-- Compiling to Html

buildHtml : Int -> Int -> Bool -> ShowZones -> LangHtml.RootedIndexedTree -> Html.Html
buildHtml w h addZones showZones (i,d) = 
  Html.div []
    [ Html.iframe
        [ Attr.srcdoc <| LangHtml.printHtml (i,d)
        , Attr.style [("width", "100%"), ("height", "100%")]
        ]
        []
    , Html.div [Attr.style [("position", "relative"), ("top", toString (-1 * h)
    ++ "px")]] [buildHtml_ addZones
    showZones d i]
    ]

buildHtml_ : Bool -> ShowZones -> LangHtml.IndexedTree -> LangHtml.NodeId -> Html.Html
buildHtml_ addZones showZones d i =
  case Utils.justGet_ ("buildHtml_ " ++ toString i) i d of
    LangHtml.TextNode text -> VirtualDom.text "" -- Is there any utility in
                                                 -- this?
    LangHtml.HtmlNode shape attrs js -> 
      -- TODO: figure out: (LangHtml.attr "draggable" "false")
      let (zones, attrs') =
        let options = optionsOf showZones in
        case (addZones, Utils.maybeRemoveFirst "zones" attrs) of
          (False, Nothing)     -> ([], attrs)
          (False, Just (_, l)) -> ([], l)
          (True, Nothing) ->
            (makeZones options shape i attrs, attrs)
          (True, Just (LangHtml.AString "none", l)) ->
            (makeZones zoneOptions0 shape i attrs, l)
          (True, Just (LangHtml.AString "basic", l)) ->
            let options' = { options | addRot <- False, addColor <- False } in
             (makeZones options' shape i attrs, l)
      in
      let children = List.map (buildHtml_ addZones showZones d) js in
  --    let mainshape = (LangHtml.html shape) (LangHtml.compileAttrs attrs) children in
      Html.div [] (List.append zones children)

--------------------------------------------------------------------------------
-- Defining Zones

--TODO: add back zones to work with HTML

-- compileAttr will throw away the trace anyway
attrNum k n    = LangHtml.compileAttr k (LangHtml.ANum (n, dummyTrace))
attrNumTr k nt = LangHtml.compileAttr k (LangHtml.ANum nt)
--TODO: might need to deal with non-numbers for styles

-- Shorthand for each type of event that we associate with zones
onMouseDown = Events.onMouseDown events.address
onMouseUp   = Events.onMouseUp   events.address
onMouseOver = Events.onMouseOver events.address
onMouseOut  = Events.onMouseOut  events.address

-- Shorthand for styling the cursor
cursorStyle s = ("cursor", LangHtml.AString s)

-- All of the events that are associated with a given zone 
zoneEvents id node zone =
    [ onMouseDown (SelectObject id node zone)
    , onMouseUp MouseUp
    , onMouseOver (turnOnCaptionAndHighlights id node zone)
    , onMouseOut turnOffCaptionAndHighlights
    ]

-- Generates a zone with the node function htmlFunc and extra attributes l
-- Example: zone Html.div ID Node Zone [ Attr.width 100, ... ]
zone htmlFunc id node zoneName l =
    htmlFunc (zoneEvents id node zoneName ++ l) []

-- A lookup table for the cursor styles for each zone
cursorOfZone zone = if
  -- div zones
  | zone == "Interior"       -> cursorStyle "move"
  | zone == "RightEdge"      -> cursorStyle "ew-resize"
  | zone == "BotRightCorner" -> cursorStyle "nwse-resize"
  | zone == "BotEdge"        -> cursorStyle "ns-resize"
  | zone == "BotLeftCorner"  -> cursorStyle "nesw-resize"
  | zone == "LeftEdge"       -> cursorStyle "ew-resize"
  | zone == "TopLeftCorner"  -> cursorStyle "nwse-resize"
  | zone == "TopEdge"        -> cursorStyle "ns-resize"
  | zone == "TopRightCorner" -> cursorStyle "nesw-resize"
  -- default
  | otherwise                -> cursorStyle "default"

---- Stuff for Basic Zones -------------------------------------------------------

---- TODO use zone

-- Styles the zone to be transparent with the translucent red border
zoneBorder htmlFunc id node zoneName flag show otherAttrs =
    zone htmlFunc id node zoneName
      <| LangHtml.compileAttrs <|
           [ if flag && show
               then ("border-color", LangHtml.AString "rgba(255,0,0,0.5)")
               else ("border-color", LangHtml.AString "rgba(0,0,0,0.0)")
           , ("border-width", (if flag then LangHtml.AString "5px" else LangHtml.AString "0px"))
           , ("border-style", LangHtml.AString "solid")
           , ("background-color", LangHtml.AString "rgba(0,0,0,0)")
           , cursorOfZone zoneName
           ] ++ otherAttrs

-- Actually generates the zones for a given HtmlNode
makeZones : ZoneOptions -> String -> LangHtml.NodeId -> List LangHtml.Attr -> List Html.Html
makeZones options node id attrs =
  case Debug.log "node" node of
    "div" -> 
      let mk zone x_ y_ w_ h_ =
          zoneBorder Html.div id node zone True options.showBasic 
            <|    [ ("top", LangHtml.AString (toString y_ ++ "px"))
                  , ("left", LangHtml.AString (toString x_ ++ "px"))
                  , ("width", LangHtml.AString (toString w_ ++ "px"))
                  , ("height", LangHtml.AString (toString h_ ++ "px"))
                  , ("position", LangHtml.AString "absolute")
                  ]
          [x,y,w,h] = List.map (toNum << Utils.find_ attrs) ["left", "top", "width", "height"]
          gutter = 0.125
          (x0,x1,x2)    = (x, x + gutter*w, x + (1-gutter)*w)
          (y0,y1,y2)    = (y, y + gutter*h, y + (1-gutter)*h)
          (wSlim,wWide) = (gutter*w, (1-2*gutter)*w)
          (hSlim,hWide) = (gutter*h, (1-2*gutter)*h)
      in
        [ mk "Interior"       x1 y1 wWide hWide
        , mk "RightEdge"      x2 y1 wSlim hWide
        , mk "BotRightCorner" x2 y2 wSlim hSlim
        , mk "BotEdge"        x1 y2 wWide hSlim
        , mk "BotLeftCorner"  x0 y2 wSlim hSlim
        , mk "LeftEdge"       x0 y1 wSlim hWide
        , mk "TopLeftCorner"  x0 y0 wSlim hSlim
        , mk "TopEdge"        x1 y0 wWide hSlim
        , mk "TopRightCorner" x2 y0 wSlim hSlim
        ]
    _ -> []



--makeZones : ZoneOptions -> String -> LangHtml.NodeId -> List LangHtml.Attr -> List Svg.Svg
--makeZones options shape id l =
--  case shape of

--    "rect" ->
--        let transform = maybeTransformAttr l in
--        let mk zone x_ y_ w_ h_ =
--          zoneBorder Svg.rect id shape zone True options.showBasic transform <|
--            [ attrNum "x" x_ , attrNum "y" y_
--            , attrNum "width" w_ , attrNum "height" h_
--            ]
--        in
--        let
--          [x,y,w,h]     = List.map (toNum << Utils.find_ l) ["x","y","width","height"]
--          gut           = 0.125
--          (x0,x1,x2)    = (x, x + gut*w, x + (1-gut)*w)
--          (y0,y1,y2)    = (y, y + gut*h, y + (1-gut)*h)
--          (wSlim,wWide) = (gut*w, (1-2*gut)*w)
--          (hSlim,hWide) = (gut*h, (1-2*gut)*h)
--        in
--        let zRot =
--          let c = (x + (w/2), y + (h/2)) in
--          let r = rotZoneDelta + (h/2) in
--          zoneRotate options.addRot id shape c r (maybeTransformCmds l)
--        in
--        let zColor =
--          zoneColor options.addColor id shape x y (maybeColorNumAttr "fill" l)
--        in
--          [ mk "Interior"       x1 y1 wWide hWide
--          , mk "RightEdge"      x2 y1 wSlim hWide
--          , mk "BotRightCorner" x2 y2 wSlim hSlim
--          , mk "BotEdge"        x1 y2 wWide hSlim
--          , mk "BotLeftCorner"  x0 y2 wSlim hSlim
--          , mk "LeftEdge"       x0 y1 wSlim hWide
--          , mk "TopLeftCorner"  x0 y0 wSlim hSlim
--          , mk "TopEdge"        x1 y0 wWide hSlim
--          , mk "TopRightCorner" x2 y0 wSlim hSlim
--          ] ++ zRot
--            ++ zColor

--    "circle"  -> makeZonesCircle  options id l
--    "ellipse" -> makeZonesEllipse options id l

--    "line" ->
--        let transform = maybeTransformAttr l in
--        let [x1,y1,x2,y2] = List.map (toNumTr << Utils.find_ l) ["x1","y1","x2","y2"] in
--        let (pt1,pt2) = ((x1,y1), (x2,y2)) in
--        let zLine = zoneLine id shape "Edge" options.showBasic transform pt1 pt2 in
--        let zPts = zonePoints id shape options.showBasic transform [pt1,pt2] in
--        let zRot =
--          let c = halfwayBetween_ pt1 pt2 in
--          let r = (distance_ pt1 pt2 / 2) - rotZoneDelta in
--          zoneRotate options.addRot id shape c r (maybeTransformCmds l) in
--        zLine :: zPts ++ zRot

--    "polygon"  -> makeZonesPoly options shape id l
--    "polyline" -> makeZonesPoly options shape id l

--    "path" -> makeZonesPath options.showBasic shape id l

--    _ -> []

--makeZonesCircle options id l =
--  let transform = maybeTransformAttr l in
--  let [cx,cy,r] = List.map (toNum << Utils.find_ l) ["cx","cy","r"] in
--  let attrs = [ attrNum "cx" cx, attrNum "cy" cy, attrNum "r" r ] in
--     [zoneBorder Svg.circle id "circle" "Edge" True options.showBasic attrs transform]
--  ++ [zoneBorder Svg.circle id "circle" "Interior" False options.showBasic attrs transform]
--  ++ (zoneRotate options.addRot id "circle" (cx,cy) (r + rotZoneDelta) (maybeTransformCmds l))
--  ++ (zoneColor options.addColor id "circle" (cx - r) (cy - r) (maybeColorNumAttr "fill" l))

--makeZonesEllipse options id l =
--  let transform = maybeTransformAttr l in
--  let [cx,cy,rx,ry] = List.map (toNum << Utils.find_ l) ["cx","cy","rx","ry"] in
--  let attrs = [ attrNum "cx" cx, attrNum "cy" cy, attrNum "rx" rx, attrNum "ry" ry ] in
--     [zoneBorder Svg.ellipse id "ellipse" "Edge" True options.showBasic attrs transform]
--  ++ [zoneBorder Svg.ellipse id "ellipse" "Interior" False options.showBasic attrs transform]
--  ++ (zoneRotate options.addRot id "circle" (cx,cy) (ry + rotZoneDelta) (maybeTransformCmds l))
--  ++ (zoneColor options.addColor id "ellipse" (cx - rx) (cy - ry) (maybeColorNumAttr "fill" l))

--makeZonesPoly options shape id l =
--  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
--  let transform = maybeTransformAttr l in
--  let pts = LangHtml.toPoints <| Utils.find_ l "points" in
--  let zPts = zonePoints id shape options.showBasic transform pts in
--  let zLines =
--    let pairs = Utils.adjacentPairs (shape == "polygon") pts in
--    let f (i,(pti,ptj)) = zoneLine id shape (addi "Edge" i) options.showBasic transform pti ptj in
--    Utils.mapi f pairs in
--  let zInterior =
--    zoneBorder Svg.polygon id shape "Interior" False options.showBasic transform [
--        LangHtml.compileAttr "points" (LangHtml.APoints pts)
--      ] in
--  let zRot =
--    let (((x0,_),(y0,_))::_) = pts in
--    zoneColor options.addColor id shape x0 y0 (maybeColorNumAttr "fill" l) in
--  let firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs) in
--  if | shape == "polygon" -> zInterior :: (zLines ++ zPts ++ zRot)
--     | firstEqLast pts    -> zInterior :: (zLines ++ zPts ++ zRot)
--     | otherwise          -> zLines ++ zPts ++ zRot

--makeZonesPath showZones shape id l =
--  let _ = Utils.assert "makeZonesPoly" (shape == "path") in
--  let transform = maybeTransformAttr l in
--  let cmds = fst <| LangHtml.toPath <| Utils.find_ l "d" in
--  let (mi,pt) +++ acc = case mi of {Nothing -> acc; _ -> pt :: acc} in
--  let pts =
--    List.foldr (\c acc -> case c of
--      LangHtml.CmdZ   s              -> acc
--      LangHtml.CmdMLT s pt           -> pt +++ acc
--      LangHtml.CmdHV  s n            -> acc
--      LangHtml.CmdC   s pt1 pt2 pt3  -> pt1 +++ (pt2 +++ (pt3 +++ acc))
--      LangHtml.CmdSQ  s pt1 pt2      -> pt1 +++ (pt2 +++ acc)
--      LangHtml.CmdA   s a b c d e pt -> pt +++ acc) [] cmds
--  in
--  zonePoints id shape showZones transform pts


--------------------------------------------------------------------------------
-- User Interface

strTitle = " sketch-n-sketch " ++ params.strVersion

colorDebug_ c1 c2 =
  if | params.debugLayout -> GE.color c1
     | otherwise          -> GE.color c2

colorDebug c1 = colorDebug_ c1 interfaceColor

codebox : Int -> Int -> Model -> GE.Element
codebox w h model =
  let event =
    case model.mode of
      SyncSelect _ _ -> []
      _ -> [Events.on "input" Events.targetValue
              (Signal.message events.address << CodeUpdate)]
  in
    codebox_ w h event model.code (not (editingMode model))

highlightThisIf b =
  if b
  then ("box-shadow", "inset 0 0 10px 4px rgba(231, 76, 60,0.5)")
  else ("box-shadow", "inset 0 0 10px 4px darkgray")

codebox_ w h event s readOnly =
  let innerPadding = 4
  in
    Html.toElement w h <|
      Html.textarea
        ([ Attr.id "editor"
         , Attr.spellcheck False
         , Attr.readonly readOnly
         , Attr.style
             [ ("font-family", params.mainSection.codebox.font)
             , ("font-size", params.mainSection.codebox.fontSize)
             , ("border", params.mainSection.codebox.border)
             , ("whiteSpace", "pre")
             , ("height", "100%")
             , ("width", "100%") 
             , ("resize", "none")
             , ("overflow", "auto")
             -- Horizontal Scrollbars in Chrome
             , ("word-wrap", "normal")
             , ("background-color", "whitesmoke")
             , ("padding", toString innerPadding ++ "px")
             -- Makes the 100% for width/height work as intended
             , ("box-sizing", "border-box")
             , highlightThisIf (not readOnly)
             ]
         , Attr.value s
         , Events.onMouseUp events.address MouseUp
         ] ++ event)
        []

canvas : Int -> Int -> Model -> GE.Element
canvas w h model =
  case model.mode of
    Print s -> codebox_ w h [] s True
    _       -> canvas_ w h model

canvas_ w h model =
  let addZones = case (editingMode model, model.mode) of
    (False, AdHoc)  -> True
    (False, Live _) -> True
    _               -> False
  in
  let document = buildHtml w h addZones model.showZones model.slate in
  Html.toElement w h document
   -- Html.iframe
   --   [ Attr.srcdoc <| LangHtml.printHtml model.slate 
   --   , Attr.style [("width", "100%"), ("height", "100%")]
   --   ]
   --   []

middleWidgets w h wWrap hWrap model =
  let twoButtons b1 b2 =
    let delta = 3 in
    let wHalf = (w//2 - delta) in
    GE.flow GE.right [ b1 wHalf h, GE.spacer (2 * delta) h, b2 wHalf h ]
  in
  List.map (GE.container wWrap hWrap GE.middle) <|
    case (editingMode model, model.mode) of
      (False, SyncSelect i options) ->
        [ gapWidget w h
        , gapWidget w h
        , prevButton i w h
        , chooseButton i options w h
        , nextButton i options w h
        ]
      (False, Print _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , saveButton model w h
        , saveAsButton model w h
        , loadButton model w h
        , twoButtons (undoButton model) (redoButton model)
        ]
      (False, _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , saveButton model w h
        , saveAsButton model w h
        , loadButton model w h
        , twoButtons (undoButton model) (redoButton model)
        , gapWidget w h
        , zoneButton model w h
        -- , frozenButton model w h
        , modeButton model w h
        ] ++ (syncButton_ w h model)
      (True, _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , saveButton model w h
        , saveAsButton model w h
        , loadButton model w h
        ]

gapWidget w h = GE.spacer w h

syncButton_ w h model =
  case model.mode of
    AdHoc -> [syncButton w h]
    _     -> []

wBtn = params.mainSection.widgets.wBtn
hBtn = params.mainSection.widgets.hBtn

buttonAttrs w h =
  Attr.style
    [ ("width", dimToPix w)
    , ("height", dimToPix h)
    , ("font-family", params.mainSection.widgets.font)
    , ("font-size", params.mainSection.widgets.fontSize)
    ]

gutterForResizing orient w h =
  let s = if orient == Vertical then "ew-resize" else "ns-resize" in
  colorDebug Color.darkBlue <|
    Html.toElement w h <|
      Html.div
          [ Events.onMouseDown events.address StartResizingMid
          , Events.onMouseUp events.address MouseUp
          , Attr.style
              [ ("width", dimToPix w) , ("height", dimToPix h)
              , ("cursor", s) ]
          ]
          [ ]

-- Makes a div appropriate for the Ace code editor to be inserted into
codeBox : Int -> Int -> GE.Element
codeBox w h = Html.toElement w h <|
    Html.Lazy.lazy2 (\a b -> Html.div [ Attr.id "editor"
             , Attr.style
                 [ ("width", "100%") -- The toElement makes a wrapping Div that
                                     -- has the appropriate w/h
                 , ("height", "100%")
                 , ("pointer-events", "auto")
                 , ("z-index", "1")
                 ]
             ] []) w h

mainSectionVertical : Int -> Int -> Model -> GE.Element
mainSectionVertical w h model =
  let
    wGut    = params.mainSection.vertical.wGut
    wMiddle = wBtn
    wCode_  = (w - wMiddle - wGut - wGut) // 2
    wCode   = wCode_ + model.midOffsetX
    wCanvas = wCode_ - model.midOffsetX
    hCanvas = h - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    hWidget = params.mainSection.widgets.hBtn
                + params.mainSection.vertical.hExtra
  in

  let codeSection = if model.basicCodeBox 
                       then codebox wCode h model
                       else codeBox wCode h in

  let canvasSection =
    GE.size wCanvas h <|
      GE.flow GE.down
        [ canvas wCanvas hCanvas model
        , GE.flow GE.left
            [ colorDebug Color.red <|
                GE.container wBtn (hZInfo+1) GE.middle <|
                outputButton model wBtn hBtn
            , caption model (wCanvas+1-wBtn) (hZInfo+1) -- NOTE: +1 is a band-aid
            ]
        -- , caption model (wCanvas+1) hZInfo -- NOTE: +1 is a band-aid
        ]
  in

  let gutter = gutterForResizing model.orient wGut h in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size wMiddle h <|
        GE.flow GE.down <|
          middleWidgets wBtn hBtn wMiddle hWidget model in
  GE.flow GE.right <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

mainSectionHorizontal : Int -> Int -> Model -> GE.Element
mainSectionHorizontal w h model =
  let
    hGut    = params.mainSection.horizontal.hGut
    hMiddle = hBtn
    hCode_  = (h - hMiddle - hGut - hGut) // 2
    hCode   = hCode_ + model.midOffsetY
    hCanvas = hCode_ - model.midOffsetY - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    wWidget = params.mainSection.widgets.wBtn
                + params.mainSection.horizontal.wExtra
  in

  let codeSection = if model.basicCodeBox
                       then codebox w hCode model
                       else codeBox w hCode in

  let canvasSection =
    GE.size w (hCanvas + hZInfo) <|
      GE.flow GE.down
        [ canvas w hCanvas model
        , GE.flow GE.left
            [ colorDebug Color.red <|
                GE.container wBtn (hZInfo+1) GE.middle <|
                outputButton model wBtn hBtn
            , caption model (w-wBtn) (hZInfo+1) -- NOTE: +1 is a band-aid
            ]
        -- , caption model w (hZInfo+1) -- NOTE: +1 is a band-aid
        ]
  in

  let gutter = gutterForResizing model.orient w hGut in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size w hMiddle <|
        GE.flow GE.right <|
          middleWidgets wBtn hBtn wWidget hMiddle model in
  GE.flow GE.down <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

simpleButton_
   : Signal.Address a -> a -> Bool -> a -> String -> String -> String
  -> Int -> Int -> GE.Element
simpleButton_ addy defaultMsg disabled msg value name text w h =
  if disabled then 
      GI.customButton (Signal.message addy defaultMsg)
        (makeButton Disabled w h text)
        (makeButton Disabled w h text)
        (makeButton Disabled w h text)
  else
      GI.customButton (Signal.message addy msg)
        (makeButton Raised w h text)
        (makeButton Highlighted w h text)
        (makeButton Depressed w h text)

simpleEventButton_ = simpleButton_ events.address Noop
simpleTaskButton_  = simpleButton_ taskMailbox.address (Task.succeed ())

simpleButton = simpleEventButton_ False
simpleTaskButton = simpleTaskButton_ False

editRunButton model w h =
  let disabled = model.mode == AdHoc in
  case editingMode model of
    True  -> simpleEventButton_ disabled Run "Run" "Run" "Run Code" w h
    False -> simpleEventButton_ disabled Edit "Edit" "Edit" "Edit Code" w h

outputButton model w h =
  let disabled = model.mode == AdHoc in
  let cap =
     case model.mode of
       Print _ -> "[Out] HTML"
       _       -> "[Out] Canvas"
  in
  simpleEventButton_ disabled ToggleOutput "Toggle Output" "Toggle Output" cap w h

syncButton =
  simpleButton Sync "Sync" "Sync the code to the canvas" "Sync"

zoneButton model =
  let cap =
    if | model.showZones == showZonesNone  -> "[Zones] Hidden"
       | model.showZones == showZonesBasic -> "[Zones] Basic"
       | model.showZones == showZonesRot   -> "[Zones] Rotation"
       | model.showZones == showZonesColor -> "[Zones] Color"
  in
  simpleButton ToggleZones "ToggleZones" "Show/Hide Zones" cap

{-
frozenButton model =
  let cap = if model.syncOptions.thawedByDefault then "[Default] n?" else "[Default] n!" in
  simpleButton ToggleThawed "ToggleThawed " "Toggle ?/!" cap
-}

chooseButton i (n,_) =
  let cap =
    if i == n + 2 then "Revert"
    else "Select " ++ Utils.parens (toString i ++ "/" ++ toString (n+1))
  in
  simpleButton SelectOption "Choose" "Choose" cap

prevButton i =
  let enabled = i > 1 in
  simpleEventButton_ (not enabled) (TraverseOption -1) "Prev" "Prev" "Show Prev"

nextButton i (n,l) =
  let enabled = i < n + 2 in
  simpleEventButton_ (not enabled) (TraverseOption 1) "Next" "Next" "Show Next"

saveButton : Model -> Int -> Int -> GE.Element
saveButton model w h =
    let disabled = List.any ((==) model.exName << fst) Examples.list
        dn = "Save"
    in simpleTaskButton_ disabled (saveStateLocally model.exName False model) dn dn dn w h

saveAsButton : Model -> Int -> Int -> GE.Element
saveAsButton model w h = 
    let dn = "Save As"
    in simpleTaskButton (saveStateLocally model.exName True model) dn dn dn w h

loadButton : Model -> Int -> Int -> GE.Element
loadButton model w h =
    simpleTaskButton (loadLocalState model.exName) "Reload" "Reload" "Reload" w h

undoButton : Model -> Int -> Int -> GE.Element
undoButton model =
  let past = fst model.history in
  simpleEventButton_ (List.length past == 0) Undo "Undo" "Undo" "Undo"

redoButton : Model -> Int -> Int -> GE.Element
redoButton model =
  let future = snd model.history in
  simpleEventButton_ (List.length future == 0) Redo "Redo" "Redo" "Redo"

dropdownExamples : Model -> Int -> Int -> GE.Element
dropdownExamples model w h =
  let 
    choices = case model.mode of
      AdHoc -> [(model.exName, Signal.send events.address Noop)]
      _ ->
        let foo (name,thunk) = (name, Signal.send events.address (SelectExample name thunk)) 
            bar saveName = (saveName, loadLocalState saveName)
            blank = ("", Task.succeed ())
            localsaves = case model.localSaves of
                [] -> []
                l  -> 
                  List.concat
                    [ [ ("Local Saves:", Task.succeed ())
                      , blank
                      ]
                    , List.map bar l
                    , [ blank ]
                    ]
        in List.concat
            [ localsaves
            , [ ("Builtin Examples:", Task.succeed ())
              , blank
              ]
            , (List.map foo Examples.list)
            , [ blank
              , ("*Clear Local Saves*", clearLocalSaves)
              ]
            ]
    options = List.map (\(name,task) -> 
        if | name == model.exName ->
              Html.option
                [ Attr.value name
                , Attr.selected True
                ] 
                [ Html.text name ]
           | otherwise ->
              Html.option
                [ Attr.value name
                ] 
                [ Html.text name ]) choices
    findTask name choices = case choices of
        (n,t) :: rest -> if | n == name -> t
                            | otherwise -> findTask name rest
        [] -> Debug.crash "Dropdown example does not have associated task"
  in Html.toElement 120 24 <| Html.select 
        [ Attr.style 
          [ ("pointer-events", "auto")
          , ("border", "0 solid")
          , ("display", "block")
          , ("width", "120px")
          , ("height", "24px")
          , ("font-family", "sans-serif")
          , ("font-size", "1em")
          ] 
        , Events.on "change" Events.targetValue 
                (\selected -> Signal.message taskMailbox.address <|
                                findTask selected choices)
        ] options

modeButton model =
  if model.mode == AdHoc
  then simpleEventButton_ True Noop "SwitchMode" "SwitchMode" "[Mode] Ad Hoc"
  else simpleEventButton_ False (SwitchMode AdHoc) "SwitchMode" "SwitchMode" "[Mode] Live"

orientationButton w h model = 
    let text = "[Orientation] " ++ toString model.orient
    in
      simpleButton SwitchOrient text text text w h

basicBoxButton w h model =
    let (text,flip) = case model.basicCodeBox of
          True  -> ("[Code Box] Basic", False)
          False -> ("[Code Box] Fancy", True)
    in
       simpleButton 
         (SetBasicCodeBox flip)
         text text text w h

--------------------------------------------------------------------------------
-- Zone Caption and Highlights

caption : Model -> Int -> Int -> GE.Element
caption model w h =
  let eStr = GE.leftAligned << T.color Color.white << T.monospace << T.fromString in
  colorDebug Color.orange <|
    GE.container w h GE.topLeft <|
      case (model.caption, model.mode, model.mouseMode) of
        --(Just (Hovering (i,k,z)), Live info, MouseNothing) ->
        --  case hoverInfo info (i,k,z) of
        --    Nothing -> GE.empty
        --    Just l ->
        --      let numLocs = List.map (\(s,n) -> toString n.val ++ Utils.braces s) l in
        --      let line1 = (k ++ toString i) ++ " " ++ z in
        --      let line2 = Utils.spaces numLocs in
        --      eStr (" " ++ line1 ++ "\n " ++ line2)
        (Just (LangError err), _, _) ->
          eStr err
        _ ->
          GE.empty

-- this is a bit redundant with Model.liveInfoToHighlights...
hoverInfo info (i,k,z) =
  let err y = "hoverInfo: " ++ toString y in
  flip Utils.bindMaybe (Dict.get i info.assignments) <| \d ->
  flip Utils.bindMaybe (Dict.get z d)                <| \(locset,_) ->
    let locs = Set.toList locset in
    Just <|
      List.map (\(lid,_,x) ->
        let n = Utils.justGet_ (err (i,z,lid)) lid info.initSubst in
        if | x == ""   -> ("loc_" ++ toString lid, n)
           | otherwise -> (x, n)) locs

turnOnCaptionAndHighlights id shape zone =
  UpdateModel <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    let hi = liveInfoToHighlights id zone m in
    { m | caption <- Nothing --TODO: Just (Hovering (id, shape, zone))
        , codeBoxInfo <- { codeBoxInfo | highlights <- hi } }

turnOffCaptionAndHighlights =
  UpdateModel <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    { m | caption <- Nothing
        , codeBoxInfo <- { codeBoxInfo | highlights <- [] } }

--------------------------------------------------------------------------------

-- The pop-up save dialog box
-- TODO clean this up, is needlessly bulky
saveElement : Model -> Int -> Int -> GE.Element
saveElement model w h = case model.mode of
  SaveDialog x -> 
      -- Note that dimBox must not be a parent of the pickBox, as
      -- opacity of a parent clobbers that of all its children
      let dimBox = GE.color Color.black
                      <| GE.opacity 0.5
                      <| GE.spacer w h
          pickBox = GE.container w h GE.middle  
                      <| GE.color interfaceColor
                      <| GE.container 400 200 GE.middle
                      <| GE.flow GE.down
                           [ GE.flow GE.right
                              [ GE.spacer 42 18 
                              , GE.centered <|
                                  T.style titleStyle
                                  (T.fromString "Save Work to Browser")
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                              [ Html.toElement 200 40
                                  <| Html.input
                                      [ Attr.type' "text"
                                      , Attr.style 
                                          [ ("height", "32px")
                                          , ("width", "192px")
                                          , ("padding", "4px")
                                          , ("border-width", "0px")
                                          , ("pointer-events", "auto")
                                          , ("box-shadow", "inset 0 0 10px 3px lightgray")
                                          ]
                                      , Attr.value model.fieldContents.value
                                      , Attr.placeholder
                                            model.fieldContents.hint
                                      , Attr.autofocus True
                                      , Events.on "input" Events.targetValue
                                          (\cont -> Signal.message events.address
                                            <| UpdateModel
                                              (\model ->
                                                  { model | fieldContents <-
                                                              { value = cont
                                                              , hint =
                                                                  model.fieldContents.hint }  
                                                  }
                                              )
                                          )
                                      ]
                                      []
                              , GE.spacer 10 40
                              , simpleTaskButton
                                  ( checkAndSave model.fieldContents.value
                                                 model
                                  )
                                  "Create Save" "Create Save" "Create Save"
                                  100 40
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                              [ GE.spacer 47 50 
                              , GE.centered <|
                                  T.height 12 <|
                                  T.color Color.white <|
                                  (T.fromString <| 
                                  "Note: This will overwrite saves with\n"
                                  ++ "the same name. You must choose a\n"
                                  ++ "name different than a built-in example.")
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                               [ GE.spacer 112 30 
                               , simpleButton
                                  (RemoveDialog False "")
                                  "Cancel" "Cancel" "Cancel"
                                  75 30
                               ]
                           ]
      in GE.flow GE.outward [ dimBox, pickBox ]
  _ -> GE.empty 
    

view : (Int, Int) -> Model -> GE.Element
view (w,h) model =
  let
    wAll = w - (2 * wGut) - 1
    wGut = params.wGut
    hTop = params.topSection.h
    hBot = params.botSection.h
    hMid = h - hTop - hBot - 1
    hTot = hTop + hMid + hBot
  in

  let topSection =
    let
      title = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <| 
                GE.leftAligned <| T.style titleStyle (T.fromString strTitle)

      wLogo = params.topSection.wLogo
      logo  = GE.image wLogo wLogo "light_logo.svg"

      wBtnO = params.topSection.wBtnO
      hBtnO = params.topSection.hBtnO
      wJunk = params.topSection.wJunk
      wSpcB = 15

      wSep  = GE.spacer (wAll - (wLogo + 2 * wBtnO + wJunk + wSpcB)) 1
      btnO  = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                orientationButton wBtnO hBtnO model
      spcB  = GE.spacer wSpcB hTop
      btnB  = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                basicBoxButton wBtnO hBtnO model
    in
      GE.size wAll hTop <|
        GE.flow GE.right
          [ GE.container wLogo hTop GE.middle logo
          , GE.container (wAll - wLogo) hTop GE.middle <|
              GE.flow GE.right [ title, wSep, btnB, spcB, btnO ]
          ]
  in

  let midSection =
    GE.size wAll hMid <|
      case model.orient of
        Vertical   -> mainSectionVertical wAll hMid model
        Horizontal -> mainSectionHorizontal wAll hMid model in

  let botSection = GE.spacer wAll hBot in
  let sideGutter = colorDebug Color.black <| GE.spacer wGut hTot in

  let basicUI =
    GE.flow GE.right
       [ sideGutter
       , GE.flow GE.down
           [ colorDebug Color.lightYellow <| topSection
           , midSection
           , colorDebug Color.lightYellow <| botSection
           ]
       , sideGutter
       ]
  in

-- Investigation into what exactly causes the blank out when Save As or
-- an orientation change happens. For the Save As, it sppears that the extra
-- GE.flow GE.inward is the culprit, but it's not definitive yet. 
--  case (model.startup, model.mode) of
--    (True, _) ->
--      let foo _ =
--        Signal.message taskMailbox.address <|
--          -- Insert more tasks to run at startup here
--          getLocalSaves `andThen` \_ ->
--          Signal.send
--            events.address
--            (UpdateModel (\m -> { m | startup <- False}))
--      in
--      GE.flow GE.inward
--        [ GI.hoverable foo <| GE.spacer w h
--        , basicUI
--        ]
--    (False, SaveDialog m) ->
--        GE.flow GE.inward 
--          [ saveElement model w h
--          ,                  
--        GE.flow GE.down
--           [ colorDebug Color.lightYellow topSection
--           , GE.flow GE.right
--                [ sideGutter
--                , midSection
--                , sideGutter
--                ]
--           , colorDebug Color.lightYellow botSection
--           ]
--           ]
--    _ ->
--    GE.flow GE.right
--       [ sideGutter
--       , GE.flow GE.down
--           [ colorDebug Color.lightYellow <| topSection
--           , midSection
--           , colorDebug Color.lightYellow <| botSection
--           ]
--       , sideGutter
--       ]

  -- Runs a task at startup by making the whole window hoverable briefly, which
  -- fires the task to the taskMailbox basically right away (the user's mouse is
  -- presumably over the window). Note that it is important to add the event
  -- handler to a dummy object that is removed, as adding it to the whole body
  -- results in nothing being clickable after the load is successful.
  case (model.startup, model.mode) of
    (True, _) ->
      let foo _ =
        Signal.message taskMailbox.address <|
          -- Insert more tasks to run at startup here
          getLocalSaves `andThen` \_ ->
          Signal.send
            events.address
            (UpdateModel (\m -> { m | startup <- False}))
      in
      GE.flow GE.inward
        [ GI.hoverable foo <| GE.spacer w h
        , basicUI
        ]
    (False, SaveDialog m) ->
      GE.flow GE.inward
        [ saveElement model w h
        , basicUI
        ]
    _ ->
      basicUI

-- TODO: add onMouseUp DeselectObject event to all GE.Elements...

------------------------------------------------------------------------------