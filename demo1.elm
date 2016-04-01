import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window
import Plasma

main =
  Signal.map3 draw Window.width Window.height (Signal.foldp (+) 0 (fps 30))

draw w h t =
    if ((t/1000) < 10) then
        chilimatrix w h t
    else if ((t/1000) < 40) then
        rotozoom w h t
    else if ((t/1000) < 70) then
        hypnocorn w h t
    else if ((t/1000) < 80) then
        Plasma.plasma w h t
    else
        chilicorn w h t


chilimatrix w h t =
  collage w h
    [ tiledImage (w * 5) (h * 5) "chilicorn.png"
        |> toForm
        |> scale(0.4)
        |> alpha ((t/4000))
    ]

chilicorn w h t =
    collage w h
      [ image h h "chilicorn.png"
        |> toForm
        |> scale (0.33)
        |> alpha ((t/1000))
      ]

rotozoom w h t =
  collage w h
    [ tiledImage (w * 5) (h * 5) "chilicorn.png"
        |> toForm
        |> scale (1.33 + cos(t/1000))
        |> rotate (degrees ((sin(t/1000) * 100) + (cos(t/2000)) * 150))
        |> move ((sin(t/1000) * 150), (sin(t/1000) * 150))
    ]


hypnocorn w h t =
  collage w h
    [ ngon 5 (toFloat w/12)
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
