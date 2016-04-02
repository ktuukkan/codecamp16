module Hypnocorn(hypnocorn, main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window

main =
  Signal.map3 hypnocorn Window.width Window.height (Signal.foldp (+) 0 (fps 30))

hypnocorn w h t =
  collage w h
    [ ngon 5 (toFloat w/13)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -5))
        |> alpha(t/500)
    , ngon 6 (toFloat w/11)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -25))
        |> alpha(t/1000)
    , ngon 7 (toFloat w/9)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -45))
        |> alpha(t/1500)
    , ngon 8 (toFloat w/8)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -65))
        |> alpha(t/2000)
    , ngon 9 (toFloat w/7)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -85))
        |> alpha(t/2500)
    , ngon 10 (toFloat w/6)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -105))
        |> alpha(t/3000)
    , ngon 8 (toFloat w/5)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -125))
        |> alpha(t/3500)
    , ngon 8 (toFloat w/4)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -145))
        |> alpha(t/4000)
    , ngon 9 (toFloat w/3)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -165))
        |> alpha(t/4500)
    , ngon 10 (toFloat w/2)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -185))
        |> alpha(t/5000)
    , ngon 4 (toFloat w)
        |> filled colorize
        |> alpha(t/5500)
    , image 50 50 "chilicorn.png"
        |> toForm
        |> rotate (degrees (-15 * cos(t/1000)))
        |> scale (5 + (sin(t/1000) * cos(t/1000) * 4))
        |> move (cos(t/1000) * 40, cos(t/1000) * 75)
        |> alpha(t/2000)
    ]

colorize : Color
colorize =
  rgba 0 64 0 0.30
