module Test exposing (main)
import Html exposing (div)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model =
  [
    square 1000
      |> filled red
  ]

trans0 t = move (0,40) >> rotate (pi/6*t)

trans1 t = move (100, 0) >> rotate (-pi/4*t) >> move (-100, 0)

type Msg2 = Tick Float GetKeyState

type alias Model = { time : Float }

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }
view model = collage 192 128 (myShapes model)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }
