module Sutcliffe.Update exposing (Modifier, modify, tick)

import Color exposing (rgba)
import Direction2d
import LineSegment2d
import List.Extra as List
import Perlin exposing (noise)
import Point2d
import Sutcliffe.Model exposing (Line, Model, Phase(..))
import Time exposing (Posix)


type alias Modifier =
    ()


tick : Posix -> Model -> Model
tick time model =
    let
        newModel =
            { model
                | scale = model.scale * 0.9995
                , rotation = model.rotation + 0.11
            }

        growing =
            model.growing
    in
    case model.phase of
        Struts ->
            if List.all (\line -> line.growth >= 1) growing.struts then
                { newModel
                    | growing = { growing | sides = spawnSides growing.struts }
                    , phase = Sides
                    , strutLength = model.strutLength * 1.24
                }

            else
                { newModel
                    | growing =
                        { growing
                            | struts = List.map updateGrowing growing.struts
                        }
                }

        Sides ->
            if List.all (\line -> line.growth >= 1) growing.sides then
                { newModel
                    | finished = model.finished ++ [ growing ]
                    , growing =
                        { struts = spawnStruts model.strutLength growing.sides
                        , sides = []
                        }
                    , phase = Struts
                }

            else
                { newModel
                    | growing = { growing | sides = List.map updateGrowing growing.sides }
                }


modify : Model -> Modifier -> Float -> Model
modify model mod val =
    model


updateGrowing : Line -> Line
updateGrowing line =
    if line.growth < 1 then
        { line | growth = line.growth + 0.0075 }

    else
        line


spawnSides : List Line -> List Line
spawnSides lines =
    let
        endPoints =
            List.map .endpoint lines

        pairs =
            List.zip endPoints (shift endPoints)
    in
    List.map
        (\( a, b ) ->
            { origin = a
            , endpoint = b
            , growth = 0
            }
        )
        pairs


spawnStruts : Float -> List Line -> List Line
spawnStruts length lines =
    let
        endPoints =
            List.map .endpoint lines

        pairs =
            List.zip endPoints (shift endPoints)
    in
    List.map
        (\( a, b ) ->
            let
                segment =
                    LineSegment2d.fromEndpoints ( a, b )

                midpoint =
                    LineSegment2d.midpoint segment

                direction =
                    LineSegment2d.perpendicularDirection segment
                        |> Maybe.map Direction2d.reverse
                        |> Maybe.withDefault Direction2d.x
            in
            { origin = midpoint
            , endpoint = Point2d.translateIn direction length midpoint
            , growth = 0
            }
        )
        pairs


shift : List a -> List a
shift list =
    Maybe.map2
        (\head tail ->
            tail ++ [ head ]
        )
        (List.head list)
        (List.tail list)
        |> Maybe.withDefault list
