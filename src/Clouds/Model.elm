module Clouds.Model exposing (Cloud, CloudRow, Coords, Dimensions, Model, buildCloudRow, init)

import List.Extra as List
import Messages exposing (Message)
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , cloudRows : List CloudRow
    , extremity : Float
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
            [ buildCloudRow 0.5 width seed
            ]
      , extremity = 0.5
      }
    , Cmd.none
    )


type alias CloudBuildData =
    { totalWidth : Float
    , allocatedWidth : Float
    , extremity : Float
    , seed : Random.Seed
    }


buildCloudRow : Float -> Float -> Random.Seed -> CloudRow
buildCloudRow extremity windowWidth seed =
    { height = 10
    , y = 1
    , xScale = 1
    , yScale = 1
    , opacity = 0
    , clouds =
        List.unfoldr buildCloud
            { totalWidth = windowWidth
            , allocatedWidth = 0
            , extremity = extremity
            , seed = seed
            }
    }


buildCloud : CloudBuildData -> Maybe ( Cloud, CloudBuildData )
buildCloud { totalWidth, allocatedWidth, seed, extremity } =
    if allocatedWidth >= totalWidth then
        Nothing

    else
        let
            ( cloudWidth, seed1 ) =
                Random.step (Random.float 20 50) seed

            ( topLeftCorner, seed2 ) =
                Random.step (cornerGenerator extremity) seed1

            ( topRightCorner, seed3 ) =
                Random.step (cornerGenerator extremity) seed2

            ( bottomLeftCorner, seed4 ) =
                Random.step (cornerGenerator extremity) seed3

            ( bottomRightCorner, seed5 ) =
                Random.step (cornerGenerator extremity) seed4
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
              , extremity = extremity
              , totalWidth = totalWidth
              }
            )


probability : Random.Generator Float
probability =
    Random.float 0 1


cornerGenerator : Float -> Random.Generator CornerInfo
cornerGenerator extremity =
    Random.map3 CornerInfo
        -- horizontal length of curve (%)
        (Random.float (0.6 * extremity) (1 * extremity))
        -- X control point position
        (Random.float (0.8 * extremity) (extremity + 1))
        -- Y control point position
        (Random.float 0 (1.2 * extremity))
