module View exposing (draw)

import Clouds.EffectView
import Clouds.Model
import Effects
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Messages exposing (..)
import Model exposing (Model)


draw : Effect model mod -> List MetaEffect -> List (Html Message)
draw effect otherEffects =
    [ layout [ width fill, height fill ] <|
        row [ width fill, height fill ]
            [ column [ Font.size 12, width (px 200), height fill, spacing 20, padding 20 ] <|
                [ text ("Current Effect: " ++ Effects.name effect)
                , Input.radio []
                    { onChange = UserSelectedEffect
                    , options = List.map effectOption otherEffects
                    , selected = Nothing
                    , label = Input.labelAbove [] (text "Choose New Effect")
                    }
                , column [ width fill ] <|
                    List.map (modSlider effect) (Effects.modifiers effect)
                ]
            , el [ width fill, height fill ] <|
                html (Effects.draw effect)
            ]
    ]


effectOption : MetaEffect -> Input.Option MetaEffect Message
effectOption metaEffect =
    let
        -- this should be easier too!
        name =
            case metaEffect of
                CloudEffect eff ->
                    Effects.name eff

                LightningEffect eff ->
                    Effects.name eff
    in
    case metaEffect of
        CloudEffect eff ->
            Input.option metaEffect (text name)

        LightningEffect eff ->
            Input.option metaEffect (text name)


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
