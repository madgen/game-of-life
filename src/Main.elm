module Main exposing (..)

import Browser
import Browser.Events

import Json.Decode as Decode

import Array exposing (Array, initialize, indexedMap, get, set)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MAIN


main =
  Browser.element
  { init = \() -> (init, Cmd.none)
  , update = \msg model -> (update msg model, Cmd.none)
  , view = view
  , subscriptions = subscriptions
  }


-- MODEL


type alias Model = Array (Array Bool)


getCell : (Int, Int) -> Model -> Maybe Bool
getCell (x, y) model =
  Maybe.andThen (get x) (get y model)


setCell : (Int, Int) -> Bool -> Model -> Model
setCell (x, y) value model =
  case get y model of
    Just row ->
      set y (set x value row) model

    Nothing ->
      model


countNeighbours : (Int, Int) -> Model -> Int
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


isLive : (Int, Int) -> Model -> Bool
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


width : Int
width = 24


height : Int
height = 24


init : Model
init = Array.initialize height (always (Array.initialize width (always False)))


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
  | Nada


update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle x y ->
      toggle (x,y) model

    Tick ->
      tick model

    Nada ->
      model


toggle : (Int, Int) -> Model -> Model
toggle ix model =
  case getCell ix model of
    Just cell ->
      setCell ix (not cell) model

    Nothing ->
      model


tick : Model -> Model
tick board =
  indexedMap (\y row -> indexedMap (\x _ -> isLive (x,y) board) row) board


-- VIEW


view : Model -> Html Msg
view model =
  div []
  [ viewBoard model
  , p [] [ text "Press <SPACE> to animate." ]
  ]


viewBoard : Array (Array Bool) -> Html Msg
viewBoard board =
  div [ ]
  <| Array.toList (indexedMap viewRow board)


viewRow : Int -> Array Bool -> Html Msg
viewRow y =
  div
    [ style "height" "30px"
    , style "border" "1px solid white"
    ]
  << Array.toList
  << indexedMap (viewCell y)


viewCell : Int -> Int -> Bool -> Html Msg
viewCell y x cell =
  div
    [ onClick (Toggle x y)
    , style "display" "inline-block"
    , style "border" "1px solid white"
    , style "width" "30px"
    , style "height" "30px"
    , style "background" (if cell then "black" else "lightgray")
    ]
    [ ]
