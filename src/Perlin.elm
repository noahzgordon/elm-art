module Perlin exposing (noise)

-- https://flafla2.github.io/2014/08/09/perlinnoise.html

import Bitwise
import List.Extra as List
import Random


noise : ( Float, Float, Float ) -> Random.Seed -> Float
noise ( x, y, z ) seed0 =
    let
        ( permutation, seed1 ) =
            Random.step
                (Random.list 512
                    (Random.int 0 255)
                )
                seed0

        p : Int -> Int
        p int =
            List.getAt int permutation
                |> Maybe.withDefault 0

        ( xi, yi, zi ) =
            ( floor x
                |> Bitwise.and 255
            , floor y
                |> Bitwise.and 255
            , floor z
                |> Bitwise.and 255
            )

        ( xf, yf, zf ) =
            ( x - toFloat (floor x)
            , y - toFloat (floor y)
            , z - toFloat (floor z)
            )

        ( u, v, w ) =
            ( fade xf, fade yf, fade zf )

        aaa =
            p (p (p xi + yi) + zi)

        aba =
            p (p (p xi + yi + 1) + zi)

        aab =
            p (p (p xi + yi) + zi + 1)

        abb =
            p (p (p xi + yi + 1) + zi + 1)

        baa =
            p (p (p (xi + 1) + yi) + zi)

        bba =
            p (p (p (xi + 1) + yi + 1) + zi)

        bab =
            p (p (p (xi + 1) + yi) + zi + 1)

        bbb =
            p (p (p (xi + 1) + yi + 1) + zi + 1)

        x1 =
            lerp
                ( grad aaa ( xf, yf, zf )
                , grad baa ( xf - 1, yf, zf )
                , u
                )

        x2 =
            lerp
                ( grad aba ( xf, yf - 1, zf )
                , grad baa ( xf - 1, yf, zf )
                , u
                )

        y1 =
            lerp ( x1, x2, v )

        x1_ =
            lerp
                ( grad aab ( xf, yf - 1, zf - 1 )
                , grad bab ( xf - 1, yf, zf - 1 )
                , u
                )

        x2_ =
            lerp
                ( grad abb ( xf, yf - 1, zf - 1 )
                , grad bbb ( xf - 1, yf - 1, zf - 1 )
                , u
                )

        y2 =
            lerp ( x1_, x2_, v )
    in
    (lerp ( y1, y2, w ) + 1) / 2


fade : Float -> Float
fade t =
    t * t * t * (t * (t * 6 - 15) + 10)


grad : Int -> ( Float, Float, Float ) -> Float
grad hash ( x, y, z ) =
    case Bitwise.and hash 0x0F of
        0x00 ->
            x + y

        0x01 ->
            negate x + y

        0x02 ->
            x - y

        0x03 ->
            negate x - y

        0x04 ->
            x + z

        0x05 ->
            negate x + z

        0x06 ->
            x - z

        0x07 ->
            negate x - z

        0x08 ->
            y + z

        0x09 ->
            negate y + z

        0x0A ->
            y - z

        0x0B ->
            negate y - z

        0x0C ->
            y + x

        0x0D ->
            negate y + z

        0x0E ->
            y - x

        0x0F ->
            negate y - z

        _ ->
            0


lerp : ( Float, Float, Float ) -> Float
lerp ( a, b, x ) =
    a + x * (b - a)
