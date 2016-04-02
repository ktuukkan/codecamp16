module Credits(credits, main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Basics exposing (..)
import Window
import Text exposing (..)

main =
  Signal.map3 credits Window.width Window.height (Signal.foldp (+) 0 (fps 30))

credits w h t =
    if (t < 33000) then
        collage w h
            [ rect (toFloat w) (toFloat h)
                |> filled black
            ,image h h "chilicorn.png"
                |> toForm
                |> scale (0.33)
                --|> alpha (1.5 - sin(t/1000))
                |> rotate (degrees (cos(t/1000)*10))
            ,
            textElement t
            ]
    else
        collage w h
            [ rect (toFloat w) (toFloat h)
                |> filled black
            ]

textElement : Float -> Form
textElement t =
    text (monospace (Text.color grey (message t)))
        |> scale(3)
        |> move (cos(t/1000)*200, -200 + sin(t/150)*10)

message : Float -> Text
message t =
    if t < 4000 then
        fromString "-=[ FTRC - Code-Camp '16 ]=-"
    else if t < 8000 then
        fromString "-=[ starfield - mmyn & aahv ]=-"
    else if t < 12000 then
        fromString "-=[ rotocorn - ktuu ]=-"
    else if t < 16000 then
        fromString "-=[ hypnocorn - ktuu ]=-"
    else if t < 20000 then
        fromString "-=[ plasma - aahv ]=-"
    else if t < 24000 then
        fromString "-=[ squarething - mmyn ]=-"
    else if t < 28000 then
        fromString "-=[ greetz to Hummeripojat! ]"
    else
        fromString "chilicorn.org"
