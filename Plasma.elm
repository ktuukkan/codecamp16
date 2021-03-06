module Plasma(plasma, main) where

import Graphics.Element exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (..)
import WebGL exposing (..)
import Window exposing (width, height)

-- Create a mesh with two triangles

type alias Vertex = { position : Vec3, color : Vec3 }


mesh : Drawable Vertex
mesh = Triangle
  [ ( Vertex (vec3 0 1 0) (vec3 1 0 0)
    , Vertex (vec3 1 1 0) (vec3 0 1 0)
    , Vertex (vec3 1 0 0) (vec3 0 0 1)
    ),
    ( Vertex (vec3 0 1 0) (vec3 1 0 0)
    , Vertex (vec3 1 0 0) (vec3 0 1 0)
    , Vertex (vec3 0 0 0) (vec3 0 0 1)
    )
  ]

main : Signal Element
main =
    Signal.map3 plasma Window.width Window.height (Signal.foldp (+) 0 (fps 30))

plasma : Int -> Int -> Float -> Element
plasma w h t =
    webgl (w,h) [ render vertexShader fragmentShader mesh { perspective = (makeOrtho2D 0 1 0 1), t = t / 100 } ]

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 } { unif | perspective:Mat4, t:Float } { pos:Vec3 }
vertexShader = [glsl|
attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 pos;
void main () {
    gl_Position = perspective * vec4(position, 1.0);
    pos = position;
}
|]


fragmentShader : Shader {} { a | t:Float } { pos:Vec3 }
fragmentShader = [glsl|
precision mediump float;
uniform float t;
varying vec3 pos;

const float pi = 3.1415926;

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main () {
    vec2 c = 2.0 * (pos.xy + vec2(sin(t / 5.0), cos(t / 3.0)));

    float v1 = sin(4.0 * pos.x * pi + t / 3.0);
    float v2 = cos(5.0 * (pos.x * sin(t / 5.0) + pos.y * cos(t / 5.0)));
    float v3 = sin(2.0 * (length(c) + t / 6.0));

    float v = v1 + v2 + v3;

    gl_FragColor = vec4(hsv2rgb(vec3(v / 2.0, 0.99, 1.0)), 1.0);
}
|]
