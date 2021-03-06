module CelAnimate.Html exposing (..)

import CelAnimate.Algebra exposing (..)
import CelAnimate.Data.Encode as Encode
import DOM exposing (..)
import Html exposing (Attribute, Html, i, input, span, text)
import Html.Attributes exposing (attribute, class, property, type_, value)
import Html.Events as Events exposing (on, onClick, preventDefaultOn, stopPropagationOn, targetChecked)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe.Extra as Maybe
import Round


type alias Three msg =
    Html msg


icon : String -> Html msg
icon name =
    span [] [ i [ class <| "select-none fas fa-" ++ name ] [] ]


icon_ : String -> Html msg
icon_ name =
    span [ class "p-1" ] [ i [ class <| "select-none fas fa-" ++ name ] [] ]


slider : Float -> Float -> Float -> Int -> Float -> Html ( Bool, Float )
slider min max step round x =
    input
        [ type_ "range"
        , class "w-32"
        , value <| Round.round round x
        , Events.onInput <|
            Tuple.pair False
                << Maybe.withDefault x
                << String.toFloat
        , on "pointerup" <|
            Decode.map (Tuple.pair True) <|
                targetValue Decode.float
        , floatAttr "min" min
        , floatAttr "max" max
        , floatAttr "step" step
        ]
        []


checkbox : Bool -> Html Bool
checkbox check =
    input
        [ type_ "checkbox"
        , class """form-checkbox text-teal-700 bg-gray-800
             border-gray-700 outline-none m-1"""
        , boolAttr "checked" check
        , property "checked" <| Encode.bool check
        , onCheck identity
        ]
        []


button : String -> msg -> Html msg
button txt msg =
    Html.button
        [ class "bg-gray-800 hover:bg-gray-900 w-full my-1"
        , onClick msg
        ]
        [ text txt ]


onChange : Decode.Decoder msg -> Attribute msg
onChange dc =
    stopPropagationOn "change" <|
        Decode.map2 Tuple.pair (targetValue dc) (Decode.succeed True)


onInput : Decode.Decoder msg -> Attribute msg
onInput dc =
    stopPropagationOn "input" <|
        Decode.map2 Tuple.pair (targetValue dc) (Decode.succeed True)


onCheck : (Bool -> msg) -> Attribute msg
onCheck tagger =
    preventDefaultOn "input" <|
        Decode.map (\checked -> ( tagger checked, True )) targetChecked


onSelected : (Bool -> msg) -> Attribute msg
onSelected tagger =
    on "change" (Decode.map tagger targetSelected)


targetValue : Decode.Decoder a -> Decode.Decoder a
targetValue =
    Decode.at [ "target", "value" ]


targetData : Decode.Decoder a -> Decode.Decoder a
targetData =
    Decode.at [ "target", "data" ]


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
    property "position" <| Encode.vec3 v


rotation : Vec3 -> Attribute msg
rotation v =
    property "rotation" <| Encode.vec3 v


lookAt : Vec3 -> Attribute msg
lookAt v =
    property "lookAt" <| Encode.vec3 v


maybe : (a -> Html msg) -> Maybe a -> Html msg
maybe f =
    Maybe.unwrap (text "") f


maybe_ : Maybe (Html msg) -> Html msg
maybe_ =
    Maybe.withDefault <| text ""


ifThen : Bool -> Html msg -> Html msg
ifThen cond html =
    if cond then
        html

    else
        text ""
