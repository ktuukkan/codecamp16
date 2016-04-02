module Rotozoom(rotozoom, main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window

main =
  Signal.map3 rotozoom Window.width Window.height (Signal.foldp (+) 0 (fps 30))

rotozoom w h t =
  if t < 10000 then
      collage w h
        [ tiledImage (w * 5) (h * 5) "chilicorn.png"
            |> toForm
            |> scale (1.33 + cos(10000/1000))
            |> alpha (t/5000)
            |> rotate (degrees ((sin(10000/1000) * 100) + (cos(10000/2000)) * 150))
            |> move ((sin(t/1000) * 150), (sin(10000/1000) * 150))
        ]
  else
      collage w h
        [ tiledImage (w * 5) (h * 5) "chilicorn.png"
            |> toForm
            |> scale(1.33 + cos(t/1000))
            |> rotate (degrees ((sin(t/1000) * 100) + (cos(t/2000)) * 150))
            |> move ((sin(t/1000) * 150), (sin(t/1000) * 150))
        ]
