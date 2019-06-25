module Sutcliffe.Update exposing (Modifier, modify, tick)

import Color exposing (hsl)
import Direction2d as Direction
import LineSegment2d as LineSegment
import List.Extra as List
import Perlin exposing (noise)
import Point2d as Point
import Random
import Sutcliffe.Model as Model exposing (Line, Model, Phase(..), StrutGroup)
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

        growingStruts =
            List.map .strut growing.groups

        growingSides =
            List.map .sides growing.groups
    in
    case model.phase of
        Struts ->
            if List.all (\line -> line.growth >= 1) growingStruts then
                { newModel
                    | phase = Sides
                    , strutLength = model.strutLength * 1.5
                }

            else
                { newModel
                    | growing = { growing | groups = List.map growStruts growing.groups }
                }

        Sides ->
            if List.all (\( sideA, sideB ) -> sideA.growth >= 1 && sideB.growth >= 1) growingSides then
                let
                    seed0 =
                        Random.initialSeed (Time.posixToMillis time)

                    colorGen =
                        Random.map3 hsl
                            (Random.float 0 1)
                            (Random.float 0.5 1)
                            (Random.float 0.4 0.6)

                    ( newColor, seed1 ) =
                        Random.step colorGen seed0
                in
                { newModel
                    | finished =
                        [ growing ]
                            ++ model.finished
                    , growing =
                        { groups = newGroups seed1 model.strutLength growing.groups
                        , color = newColor
                        , pentNum = model.pentCount + 1
                        }
                    , phase = Struts
                    , pentCount = model.pentCount + 1
                }

            else
                { newModel
                    | growing = { growing | groups = List.map growSides growing.groups }
                }


modify : Model -> Modifier -> Float -> Model
modify model mod val =
    model


growStruts : StrutGroup -> StrutGroup
growStruts group =
    { group | strut = grow group.strut }


growSides : StrutGroup -> StrutGroup
growSides group =
    let
        ( sideA, sideB ) =
            group.sides

        ( embA, embB ) =
            group.embellishments
    in
    { group
        | sides = ( grow sideA, grow sideB )
        , embellishments = ( grow embA, grow embB )
    }


grow : { a | growth : Float } -> { a | growth : Float }
grow growable =
    if growable.growth < 1 then
        { growable | growth = growable.growth + 0.0075 }

    else
        growable


newGroups : Random.Seed -> Float -> List StrutGroup -> List StrutGroup
newGroups seed length groups =
    let
        ( probs, _ ) =
            Random.step (Random.list 5 <| Random.float 0 1) seed
    in
    Model.spawnGroups <|
        List.map
            (\( prob, group ) ->
                let
                    offset =
                        (prob - 0.5) / 3 * length

                    side =
                        Model.lineSegment (Tuple.first group.sides)

                    startPoint =
                        LineSegment.endPoint side

                    direction =
                        LineSegment.direction side
                            |> Maybe.withDefault Direction.x

                    perpDirection =
                        Direction.perpendicularTo direction

                    endPoint =
                        startPoint
                            |> Point.translateIn perpDirection length
                            |> Point.translateIn direction offset
                in
                { origin = startPoint
                , endpoint = endPoint
                , growth = 0
                }
            )
            (List.zip probs groups)
