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
                    if firstRow.y >= 20 then
                        [ Clouds.Model.buildCloudRow model.window.width seed0
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
            List.map (updateCloudRow model.window.height) newCloudRows
                |> List.filterMap identity
    }


updateCloudRow : Float -> CloudRow -> Maybe CloudRow
updateCloudRow windowHeight row =
    if row.y >= (windowHeight - 230) then
        Nothing

    else
        Just
            { row
                | y = (row.y + 0.2) * 1.0005
                , height = row.height + 0.012
                , scale = (row.scale + 0.00002) * 1.00002
                , opacity =
                    if row.opacity >= 1 then
                        1

                    else
                        row.opacity + 0.0008
            }
