module Noise2d.Update exposing (tick)

import Noise2d.Model exposing (Model, drawLines)
import Time exposing (Posix)


tick : Posix -> Model -> Model
tick time model =
    let
        ( newX, newY, newZ ) =
            ( model.xStart + 0.01
            , model.yStart + 0.01
            , model.zStart + 0.01
            )
    in
    { model
        | zStart = newZ
        , lines = drawLines newX newY newZ (model.window.width - 200) model.window.height model.seed
    }
