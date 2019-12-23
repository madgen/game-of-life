module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events

import Json.Decode as Decode

import Array exposing (Array, initialize, indexedMap, get, set)

import Task

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MAIN


main =
  Browser.element
  { init = \() -> init
  , update = \msg model -> (update msg model, Cmd.none)
  , view = view
  , subscriptions = subscriptions
  }


-- MODEL


type alias Model =
  { board  : Board
  , width  : Int
  , height : Int
  }


type alias Board = Array (Array Bool)


getCell : (Int, Int) -> Board -> Maybe Bool
getCell (x, y) board =
  Maybe.andThen (get x) (get y board)


setCell : (Int, Int) -> Bool -> Board -> Board
setCell (x, y) value board =
  case get y board of
    Just row ->
      set y (set x value row) board

    Nothing ->
      board


countNeighbours : (Int, Int) -> Board -> Int
countNeighbours (x,y) board =
  let
    ixs =
      [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
      , (x - 1, y)    ,             (x + 1, y)
      , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
      ]
  in List.length
  <| List.filter (\cell -> cell == Just True)
  <| List.map (\ix -> getCell ix board) ixs


isLive : (Int, Int) -> Board -> Bool
isLive ix board =
  let
    neighbourCount = countNeighbours ix board
  in
    case getCell ix board of
      Just live ->
        if live == True && (neighbourCount == 2 || neighbourCount == 3) then
          True
        else if not live && neighbourCount == 3 then
          True
        else
          False

      Nothing ->
        False


side : Int
side = 32


border : Int
border = 1


cellSide : Int
cellSide = side + border * 2


init : (Model, Cmd Msg)
init =
  ( { board = Array.initialize 0 (always (Array.initialize 0 (always False)))
    , width = 0
    , height = 0
    }
  , Task.perform boardSizeAction getViewport
  )


boardSizeAction : Viewport -> Msg
boardSizeAction { viewport } =
  SetSize
    (floor viewport.width // cellSide)
    (floor viewport.height // cellSide - 1)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
  case String.uncons string of
    Just (' ',"") -> Tick
    _ -> Nada


-- UPDATE


type Msg
  = Toggle Int Int
  | Tick
  | SetSize Int Int
  | Nada


update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle x y ->
      { model
      | board = toggle (x,y) model.board
      }

    Tick ->
      { model
      | board = tick model.board
      }

    SetSize width height ->
      { model
      | board =
        Array.initialize height (always (Array.initialize width (always False)))
      , width = width
      , height = height
      }

    Nada ->
      model


toggle : (Int, Int) -> Board -> Board
toggle ix board =
  case getCell ix board of
    Just cell ->
      setCell ix (not cell) board

    Nothing ->
      board


tick : Board -> Board
tick board =
  indexedMap (\y row -> indexedMap (\x _ -> isLive (x,y) board) row) board


-- VIEW


view : Model -> Html Msg
view model =
  div
  [ style "position" "relative"
  , style "margin" "0 auto 0 auto"
  , style "width" ((String.fromInt (model.width * cellSide)) ++ "px")
  , style "text-align" "center"
  , style "font-family" "Helvetica"
  ]
  [ div
    [ onClick Tick
    , style "padding" "10px 0 3px 0"
    , style "margin" "0 auto 2px auto"
    , style "font-weight" "bold"
    , style "cursor" "pointer"
    , style "-webkit-user-select" "none"
    , style "-moz-user-select"    "none"
    , style "-khtml-user-select"  "none"
    , style "-ms-user-select"     "none"
    ] [ text "CLICK (or press SPACE)" ]
  , viewBoard model.board
  ]


viewBoard : Board -> Html Msg
viewBoard board =
  div [ ]
  <| Array.toList (indexedMap viewRow board)


viewRow : Int -> Array Bool -> Html Msg
viewRow y =
  div
    [ style "height" (String.fromInt cellSide ++ "px")
    ]
  << Array.toList
  << indexedMap (viewCell y)


viewCell : Int -> Int -> Bool -> Html Msg
viewCell y x cell =
  div
    [ onClick (Toggle x y)
    , style "display" "inline-block"
    , style "border" (String.fromInt border ++ "px solid white")
    , style "width" (String.fromInt side ++ "px")
    , style "height" (String.fromInt side ++ "px")
    , style "background" (if cell then "black" else "lightgray")
    ]
    [ ]
