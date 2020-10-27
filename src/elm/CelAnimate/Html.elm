module CelAnimate.Html exposing (..)

import CelAnimate.Algebra exposing (..)
import DOM exposing (..)
import Html exposing (Attribute, Html, i, span, text)
import Html.Attributes exposing (attribute, class, property)
import Html.Events exposing (on)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe.Extra as Maybe


type alias Three msg =
    Html msg


icon : String -> Html msg
icon name =
    span [] [ i [ class <| "select-none fas fa-" ++ name ] [] ]


icon_ : String -> Html msg
icon_ name =
    span [ class "p-1" ] [ i [ class <| "select-none fas fa-" ++ name ] [] ]


onSelected : (Bool -> msg) -> Attribute msg
onSelected tagger =
    on "change" (Decode.map tagger targetSelected)


targetSelected : Decode.Decoder Bool
targetSelected =
    Decode.at [ "target", "selected" ] Decode.bool


targetImgSize : Decode.Decoder ( Float, Float )
targetImgSize =
    DOM.target <|
        Decode.map2 Tuple.pair
            (Decode.field "naturalWidth" Decode.float)
            (Decode.field "naturalHeight" Decode.float)


onResize : (Rectangle -> msg) -> Attribute msg
onResize tagger =
    on "resize" (Decode.map tagger <| target boundingClientRect)


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


position : Vec3 -> Attribute msg
position v =
    property "position" <| encodeVec3 v


rotation : Vec3 -> Attribute msg
rotation v =
    property "rotation" <| encodeVec3 v


lookAt : Vec3 -> Attribute msg
lookAt v =
    property "lookAt" <| encodeVec3 v


maybe : (a -> Html msg) -> Maybe a -> Html msg
maybe f =
    Maybe.unwrap (text "") f
