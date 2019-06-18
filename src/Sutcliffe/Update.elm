module Sutcliffe.Update exposing (Modifier, modify, tick)

import Color exposing (rgba)
import Perlin exposing (noise)
import Sutcliffe.Model exposing (Model)
import Time exposing (Posix)


type alias Modifier =
    ()


tick : Posix -> Model -> Model
tick time model =
    model


modify : Model -> Modifier -> Float -> Model
modify model mod val =
    model
