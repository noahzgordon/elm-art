module Effects exposing (applyModifier, build, draw, id, modConstructor, model, modifiers, name, tick, updateModel)

import Html exposing (Html)
import Messages exposing (..)
import Time exposing (Posix)


build :
    { id : String
    , name : String
    , draw : model -> Html Message
    , mods : List ( mod, String, model -> Float )
    , model : model
    , tick : Posix -> model -> model
    , modConstructor : mod -> Modifier
    , applyModifier : Effect model mod -> mod -> Float -> Effect model mod
    }
    -> Effect model mod
build config =
    Effect config


draw : Effect model mod -> Html Message
draw (Effect eff) =
    eff.draw eff.model


modifiers : Effect model mod -> List ( mod, String, model -> Float )
modifiers (Effect eff) =
    eff.mods


model : Effect model mod -> model
model (Effect eff) =
    eff.model


modConstructor : Effect model mod -> (mod -> Modifier)
modConstructor (Effect eff) =
    eff.modConstructor


tick : Effect model mod -> Posix -> Effect model mod
tick (Effect eff) time =
    Effect { eff | model = eff.tick time eff.model }


updateModel : Effect model mod -> (model -> model) -> Effect model mod
updateModel (Effect eff) fn =
    Effect <|
        { eff | model = fn eff.model }


applyModifier : Effect model mod -> mod -> Float -> Effect model mod
applyModifier ((Effect eff) as effect) mod val =
    eff.applyModifier effect mod val


id : Effect model mod -> String
id (Effect eff) =
    eff.id


name : Effect model mod -> String
name (Effect eff) =
    eff.name
