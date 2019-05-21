module Clouds.Model exposing (Cloud, CloudRow, Coords, Dimensions, Model, buildCloudRow, init)

import List.Extra as List
import Messages exposing (Message)
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , cloudRows : List CloudRow
    }


type alias CloudRow =
    { y : Float
    , height : Float
    , xScale : Float
    , yScale : Float
    , clouds : List Cloud
    , opacity : Float
    }


type alias Cloud =
    { width : Float
    , x : Float
    , corners :
        { topLeft : CornerInfo
        , topRight : CornerInfo
        , bottomLeft : CornerInfo
        , bottomRight : CornerInfo
        }
    }


type alias CornerInfo =
    { horizontalLength : Float
    , firstControlPointX : Float
    , secondControlPointY : Float
    }


type alias Dimensions =
    { width : Float, height : Float }


type alias Coords =
    { x : Float, y : Float }


type alias Flags =
    { window : Dimensions
    , time : Int
    }


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        seed =
            Random.initialSeed flags.time

        width =
            flags.window.width - 200
    in
    ( { window = flags.window
      , cloudRows =
            [ buildCloudRow width seed
            ]
      }
    , Cmd.none
    )


type alias CloudBuildData =
    { totalWidth : Float
    , allocatedWidth : Float
    , seed : Random.Seed
    }


buildCloudRow : Float -> Random.Seed -> CloudRow
buildCloudRow windowWidth seed =
    { height = 10
    , y = 1
    , xScale = 1
    , yScale = 1
    , opacity = 0
    , clouds =
        List.unfoldr buildCloud
            { totalWidth = windowWidth
            , allocatedWidth = 0
            , seed = seed
            }
    }


buildCloud : CloudBuildData -> Maybe ( Cloud, CloudBuildData )
buildCloud { totalWidth, allocatedWidth, seed } =
    if allocatedWidth >= totalWidth then
        Nothing

    else
        let
            ( cloudWidth, seed1 ) =
                Random.step (Random.float 20 50) seed

            ( topLeftCorner, seed2 ) =
                Random.step cornerGenerator seed1

            ( topRightCorner, seed3 ) =
                Random.step cornerGenerator seed2

            ( bottomLeftCorner, seed4 ) =
                Random.step cornerGenerator seed3

            ( bottomRightCorner, seed5 ) =
                Random.step cornerGenerator seed4
        in
        Just
            ( { width = cloudWidth
              , x = allocatedWidth
              , corners =
                    { topLeft = topLeftCorner
                    , topRight = topRightCorner
                    , bottomLeft = bottomLeftCorner
                    , bottomRight = bottomRightCorner
                    }
              }
            , { allocatedWidth = allocatedWidth + cloudWidth + 2
              , seed = seed5
              , totalWidth = totalWidth
              }
            )


probability : Random.Generator Float
probability =
    Random.float 0 1


cornerGenerator : Random.Generator CornerInfo
cornerGenerator =
    Random.map3 CornerInfo
        (Random.float 0.3 0.8)
        (Random.float 0.1 0.9)
        (Random.float 0.1 0.9)
