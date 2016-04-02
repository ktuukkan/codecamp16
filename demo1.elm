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
import Starfield
import Html exposing (..)
import Html.Attributes exposing (..)


type Effect = Rotozoom | Hypnocorn | Plasma | Starfield | Chilicorn

type alias State =
    { effect : Effect
    , effectStart : Float
    , time : Float
    , stars: Starfield.Stars
    }

muzak : Element
muzak =
    Html.audio [ src "muzak.mp3", preload "true", autoplay True ] []
      |> toElement 1 1

initialState : State
initialState =
    { effect = Chilicorn
    , effectStart = 0
    , time = 0
    , stars = Starfield.stars
    }

updateState : Float -> State -> State
updateState time state =
    let
        effect = selectEffect (time / 1000)
        effectStart =
            if effect /= state.effect then
                time
            else
                state.effectStart
    in
        { state |
            effect = effect,
            effectStart = effectStart,
            time = time,
            stars = Starfield.update state.time state.stars
        }

selectEffect : Float -> Effect
selectEffect t =
    if t < 15 then
        Starfield
    else if t < 45 then
        Rotozoom
    else if t < 60 then
        Hypnocorn
    else if t < 80 then
        Plasma
    else
        Chilicorn


main : Signal Element
main =
    Signal.map2 view (Signal.foldp updateState initialState (Signal.foldp (+) 0 (fps 60))) Window.dimensions


view : State -> (Int, Int) -> Element
view state (w, h) =
    let
      time = (state.time - state.effectStart)
      effectView = case state.effect of
        Hypnocorn ->
            hypnocorn w h time
        Starfield ->
            Starfield.view state.stars (w, h)
        Chilicorn ->
            chilicorn w h time
        Rotozoom ->
            rotozoom w h time
        Plasma ->
            plasma w h time
    in
      flow outward [effectView, muzak]

chilicorn w h t =
    collage w h
      [ image h h "chilicorn.png"
        |> toForm
        |> scale (0.33)
        |> alpha (0 + sin(t/1000))
      ]
