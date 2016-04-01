module Starfield where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Window
import Debug

velocity : Float
velocity = 1.01

starCount : Int
starCount = 400

type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }

bounds: Bounds
bounds =
    { minX = 0.5
    , minY = 0.5
    , maxX = 0.8
    , maxY = 0.8
    }

type alias Star = 
    { x: Float
    , y: Float
    }


type alias Stars =
    { stars : List Star
    , seed : Seed
    , time : Float
    }


stars : Stars
stars =
    { stars = []
    , seed = (initialSeed 1337)
    , time = 0.0
    }


background : Float -> Float -> Form
background w h =
    filled black (rect w h)


starToForm : (Float, Float) -> Star -> Form
starToForm (w, h) star =
    move (w * star.x, h * star.y) (filled white (square 3))


tupleToStar : (Float, Float) -> Star
tupleToStar (x, y) =
    { x = x, y = y }


addStars : List Star -> Seed -> (List Star, Seed) 
addStars stars seed =
    if (starCount - List.length stars) == 0 then
        (stars, seed)
    else
        let
            (star, newSeed) = (generateStar bounds.minX bounds.minY seed)
        in
            addStars (star :: stars) newSeed


moveStar : Star -> Star
moveStar star =
    { star |
        x = star.x * velocity,
        y = star.y * velocity
    }


filterInside : Star -> Bool
filterInside star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxX


generateStar : Float -> Float -> Seed -> (Star, Seed)
generateStar width height seed =
    let
        pair = Random.pair (Random.float -width width) (Random.float -height height)
        (coords, newSeed) = Random.generate pair seed
    in
        (tupleToStar coords, newSeed)
        

update : Float -> Stars -> Stars
update time stars =
    let
        movedStars = List.map moveStar stars.stars
        insideField = List.filter filterInside movedStars
        (updatedStars, updatedSeed) = addStars insideField stars.seed
    in
        { stars |
            stars = updatedStars,
            seed = updatedSeed,
            time = time
        }


view : Stars -> (Int, Int) -> Element
view stars (w, h) =
    let
        w' = toFloat w
        h' = toFloat h
        forms = List.map (starToForm (w', h')) stars.stars
        many = group forms
        angle = stars.time * 0.0005
        rotated = rotate (radians angle) many
    in
        collage w h ([(background w' h')] ++ [rotated])


main : Signal Element
main = 
    -- view (update 0 stars) (500, 500)
    Signal.map2 view (Signal.foldp update stars (Signal.foldp (+) 0 (fps 60))) Window.dimensions
