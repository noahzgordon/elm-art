module Noise2d.Model exposing (Model, drawLines, init)

import List.Extra as List
import Perlin
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , seed : Random.Seed
    , time : Posix
    , lines : List Line
    , xStart : Float
    , yStart : Float
    , zStart : Float
    }


type alias Dimensions =
    { width : Float, height : Float }


type alias Line =
    { x : Float
    , y : Float
    , noise : Float
    }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    let
        seed =
            Random.initialSeed flags.time
    in
    { window = flags.window
    , seed = seed
    , time = Time.millisToPosix flags.time
    , xStart = 0.01
    , yStart = 0.01
    , zStart = 0.01
    , lines = drawLines 0.01 0.01 0.01 (flags.window.width - 200) flags.window.height seed
    }


drawLines xStart yStart zStart width height seed =
    List.iterate
        (\accum ->
            if accum.y > height then
                Nothing

            else
                let
                    xs =
                        List.iterate
                            (\xAccum ->
                                if xAccum.x > width then
                                    Nothing

                                else
                                    Just
                                        { x = xAccum.x + 50
                                        , xNoise = xAccum.xNoise + 0.1
                                        }
                            )
                            { x = 25, xNoise = xStart }
                in
                Just
                    { y = accum.y + 50
                    , yNoise = accum.yNoise + 0.1
                    , lines =
                        accum.lines
                            ++ List.map
                                (\x ->
                                    { x = x.x
                                    , y = accum.y
                                    , noise = Perlin.noise ( x.xNoise, accum.yNoise, zStart ) seed
                                    }
                                )
                                xs
                    }
        )
        { y = 25, yNoise = yStart, lines = [] }
        |> List.concatMap
            (\rowData ->
                List.map
                    (\lineData ->
                        { x = lineData.x
                        , y = lineData.y
                        , noise = lineData.noise
                        }
                    )
                    rowData.lines
            )
