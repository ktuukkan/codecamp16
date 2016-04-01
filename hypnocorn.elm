import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window

main =
  Signal.map3 draw Window.width Window.height (Signal.foldp (+) 0 (fps 30))

draw w h t =
  collage w h
    [ ngon 5 (toFloat w/11)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -5))
    , ngon 6 (toFloat w/10)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -25))
    , ngon 7 (toFloat w/9)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -45))
    , ngon 8 (toFloat w/8)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -65))
    , ngon 9 (toFloat w/7)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -85))
    , ngon 10 (toFloat w/6)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -105))
    , ngon 8 (toFloat w/5)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -125))
    , ngon 8 (toFloat w/4)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -145))
    , ngon 9 (toFloat w/3)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -165))
    , ngon 10 (toFloat w/2)
        |> filled colorize
        |> rotate (degrees ((t/1000) * -185))
    , ngon 4 (toFloat w)
        |> filled colorize
    , image 50 50 "chilicorn.png"
        |> toForm
        |> rotate (degrees (-15 * cos(t/1000)))
        |> scale (5 + (sin(t/1000) * cos(t/1000) * 4))
        |> move (cos(t/1000) * 40, cos(t/1000) * 75)
    ]

colorize : Color
colorize =
  rgba 0 64 0 0.30

--rand t = fst <| generate (int 0 10) (initialSeed <| floor t)
