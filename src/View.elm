module View exposing (draw)

import Clouds.EffectView
import Clouds.Model
import Effects exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Messages exposing (..)
import Model exposing (MetaEffect(..), Model)


draw : Effect model mod -> List (Html Message)
draw effect =
    [ layout [ width fill, height fill ] <|
        row [ width fill, height fill ]
            [ column [ width (px 200), height fill, spacing 50, padding 50 ] <|
                List.map (modSlider effect) (Effects.modifiers effect)
            , el [ width fill, height fill ] <|
                html (Effects.draw effect)
            ]
    ]


modSlider effect ( modifier, label, prop ) =
    el
        [ height (px 50)
        , width (px 150)
        , centerX
        ]
    <|
        Input.slider
            [ Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color (rgb 0.5 0.5 0.5)
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = ModifierChanged (Effects.modConstructor effect modifier)
            , label = Input.labelAbove [] (text label)
            , min = 0
            , max = 1
            , value = prop (Effects.model effect)
            , thumb = Input.defaultThumb
            , step = Just 0.01
            }
