module Clouds.Update exposing (tick, updateCloudRow)

import Clouds.Model exposing (CloudRow, Model)
import Random
import Time exposing (Posix, posixToMillis)


tick : Posix -> Model -> Model
tick time model =
    let
        seed0 =
            Random.initialSeed (posixToMillis time)

        newCloudRows =
            case List.head model.cloudRows of
                Just firstRow ->
                    if firstRow.y >= 15 then
                        [ Clouds.Model.buildCloudRow model.extremity model.window.width seed0
                        ]
                            ++ model.cloudRows

                    else
                        model.cloudRows

                Nothing ->
                    -- this should never happen
                    model.cloudRows
    in
    { model
        | cloudRows =
            List.map (updateCloudRow model) newCloudRows
                |> List.filterMap identity
    }


updateCloudRow : Model -> CloudRow -> Maybe CloudRow
updateCloudRow model row =
    if row.y >= (model.window.height - 230) then
        Nothing

    else
        Just
            { row
                | y = (row.y + (0.4 * model.speed)) * max 1 (1.001 * model.speed)
                , xScale = (row.xScale + (0.001 * model.speed)) * max 1 (1.0002 * model.speed)
                , yScale = row.yScale * max 1 (1.0012 * model.speed)
                , opacity =
                    if row.opacity >= 1 then
                        1

                    else
                        row.opacity + (0.0016 * model.speed)
            }
