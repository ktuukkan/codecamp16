module SquareThing where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Debug
import List.Extra exposing (..)
import Time exposing (..)

type alias Square =
    { x : Float
    , y : Float
    , lit : Bool
    }


type alias Squares =
    List Square


initialSquares : Squares
initialSquares =
    [
    ]


squareToForm : (Float, Float) -> Square -> Form
squareToForm (w, h) sqr =
    let
        color =
            if sqr.lit then
                white
            else
                red
    in
        move (w * sqr.x, h * sqr.y) (filled color (square 10))


makeSquare : (Int, Int) -> Square
makeSquare (x, y) =
    { x = (toFloat x) * 0.02
    , y = (toFloat y) * 0.02
    , lit = True
    }


makeSquares : Squares -> Squares
makeSquares squares =
    let
        a = [-50..50]
        coords = a `andThen` \x -> a `andThen` \y -> [(x,y)]
    in
        if List.isEmpty squares then
            List.map makeSquare coords
        else
            squares

checkLit t sqr =
    let
        tt = t / 100
        x = sqr.x * 50
        y = sqr.y * 50

        lit = sin (x + tt) + cos(tt) < 0.5
    in
        { sqr |
            lit = lit
        }


updatedSquares time squares =
    List.map (checkLit time) squares


background : Float -> Float -> Form
background w h =
    filled black (rect w h)


update : Float -> Squares -> Squares
update time squares =
    let
        squares1 = makeSquares squares
        squares2 = updatedSquares time squares1
        n = 1
    in
        squares2


view : Squares -> (Int, Int) -> Element
view squares (w, h) =
    let
        w' = toFloat w
        h' = toFloat h
        forms = List.map (squareToForm (w', h')) squares
    in
        collage w h ([(background w' h')] ++ forms)


--main : Signal Element
main = 
    Signal.map2 view (Signal.foldp update initialSquares (Signal.foldp (+) 0 (fps 60))) Window.dimensions
