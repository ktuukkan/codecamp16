import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window
import Plasma exposing (..)
import Hypnocorn exposing (..)
import Rotozoom exposing (..)

main =
  Signal.map3 view Window.width Window.height (Signal.foldp (+) 0 (fps 30))

view w h t =
    if ((t/1000) < 10) then
        cornfield w h t
    else if ((t/1000) < 40) then
        rotozoom w h t
    else if ((t/1000) < 60) then
        hypnocorn w h t
    else if ((t/1000) < 80) then
        plasma w h t
    else
        chilicorn w h t

chilicorn w h t =
    collage w h
      [ image h h "chilicorn.png"
        |> toForm
        |> scale (0.33)
        |> alpha (1.05 - sin(t/1000))
      ]
