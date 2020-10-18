module CelAnimate.Html exposing (..)

import Html exposing (Attribute, Html, i, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (on)
import Json.Decode as Decode


type alias Three msg =
    Html msg


icon : String -> Html msg
icon name =
    i [ class "material-icons" ] [ text name ]


onSelected : (Bool -> msg) -> Attribute msg
onSelected tagger =
    on "change" (Decode.map tagger targetSelected)


targetSelected : Decode.Decoder Bool
targetSelected =
    Decode.at [ "target", "selected" ] Decode.bool


intAttr : String -> Int -> Attribute msg
intAttr name n =
    attribute name (String.fromInt n)


floatAttr : String -> Float -> Attribute msg
floatAttr name x =
    attribute name (String.fromFloat x)


boolAttr : String -> Bool -> Attribute msg
boolAttr name b =
    attribute name
        (if b then
            "true"

         else
            ""
        )
