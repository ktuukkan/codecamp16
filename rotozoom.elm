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
    [ tiledImage (w * 5) (h * 5) "chilicorn.png"
        |> toForm
        |> scale(1.33 + cos(t/1000))
        |> rotate (degrees ((sin(t/1000) * 100) + (cos(t/2000)) * 150))
        |> move ((sin(t/1000) * 150), (sin(t/1000) * 150))
    ]

-- rand t = fst <| generate (float 0 10) (initialSeed <| floor t)
