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


type Effect = Cornfield | Rotozoom | Hypnocorn | Plasma | Starfield | Chilicorn

type alias State =
    { effect : Effect
    , time : Float
    , stars: Starfield.Stars
    }

initialState : State
initialState =
    { effect = Chilicorn
    , time = 0
    , stars = Starfield.stars
    }

updateState : Float -> State -> State
updateState time state = 
    let
        effect = selectEffectTime (time / 1000)
    in
        { state |
            effect = effect,
            time = time 
        }

selectEffectTime : Float -> Effect
selectEffectTime t =
    if t < 10 then
        Cornfield
    else if t < 20 then
        Starfield
    else if t < 40 then
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
    case state.effect of
        Cornfield ->
            cornfield w h state.time
        Hypnocorn ->
            hypnocorn w h state.time
        Starfield ->
            Starfield.view (Starfield.update state.time state.stars) (w, h)
        Chilicorn ->
            chilicorn w h state.time
        Rotozoom ->
            rotozoom w h state.time
        Plasma ->
            plasma w h state.time

chilicorn w h t =
    collage w h
      [ image h h "chilicorn.png"
        |> toForm
        |> scale (0.33)
        |> alpha (1.05 - sin(t/1000))
      ]