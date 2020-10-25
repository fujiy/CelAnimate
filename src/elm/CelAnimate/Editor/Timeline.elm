module CelAnimate.Editor.Timeline exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import DOM
import Dict
import Html exposing (Attribute, Html, div, input, label, span, text)
import Html.Attributes as Attr exposing (class, property, style, type_)
import Html.Events as Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe
import Round
import Set


type ParameterMsg
    = Use ParameterDesc Bool
    | SetValue ParameterDesc Float


view : ParameterVector -> DataSelection -> Data -> Html Msg
view pv selection data =
    let
        mcel =
            selectedCel selection data

        usingParams =
            Maybe.map .parameters mcel
                |> Maybe.withDefault Dict.empty
                |> Dict.values
                |> List.map (\p -> p.name)
                |> Set.fromList

        params =
            parameters
                |> Array.mapToList (\p -> ( p, Set.member p.name usingParams ))

        message msg =
            case msg of
                Use desc use ->
                    ModifyData <| useParameter desc use selection

                SetValue desc value ->
                    ChangeParameter desc value
    in
    Html.map message <|
        div [ class "h-64 bg-gray-800" ] <|
            List.map
                (\( desc, using ) ->
                    let
                        value =
                            Dict.get desc.name pv
                                |> Maybe.withDefault (defaultValue desc)
                    in
                    parameter (Parameter desc value) using
                )
                params


parameter : Parameter -> Bool -> Html ParameterMsg
parameter p using =
    div [ class "bg-gray-700 m-px flex flex-row items-center" ]
        [ label [ class "w-24" ]
            [ input
                [ type_ "checkbox"
                , class """form-checkbox text-teal-700 bg-gray-800
                                border-gray-700 outline-none m-1"""
                , boolAttr "checked" using
                , property "checked" <| Encode.bool using
                , onCheck
                    (\checked -> Use p.desc checked)
                ]
                []
            , span [] [ text p.desc.name ]
            ]
        , Html.map
            (SetValue p.desc)
            (slider using (minimumValue p.desc) (maximumValue p.desc) p.value)
        ]


onCheck : (Bool -> msg) -> Attribute msg
onCheck tagger =
    Events.preventDefaultOn "input" <|
        Decode.map (\checked -> ( tagger checked, True )) Events.targetChecked


slider : Bool -> Float -> Float -> Float -> Html Float
slider using min max value =
    let
        cursorPos =
            (value - min) / (max - min) * 100

        decodeDrag =
            dragX (\x -> x * (max - min) + min)

        cursorColor =
            if using then
                "bg-teal-400"

            else
                "bg-gray-500"
    in
    div [ class "flex flex-grow flex-shrink" ]
        [ div
            [ class "h-6 flex flex-grow flex-shrink m-1 bg-gray-800"
            , Events.on "pointermove" decodeDrag
            , Events.on "pointerdown" decodeDrag
            ]
            [ div
                [ class <|
                    "h-6 p-px  relative pointer-events-none "
                        ++ cursorColor
                , style "left" <| String.fromFloat cursorPos ++ "%"
                ]
                []
            ]
        , span [ class "w-24" ]
            [ text <| Round.round 2 value ]
        ]


useParameter : ParameterDesc -> Bool -> DataSelection -> Data -> Data
useParameter desc use selection data =
    let
        update cel =
            { cel
                | parameters =
                    if use then
                        Dict.insert desc.name desc cel.parameters

                    else
                        Dict.remove desc.name cel.parameters
            }
    in
    updateCel selection update data


dragX : (Float -> Float) -> Decoder Float
dragX f =
    Decode.map3
        (\event pressure rect ->
            if pressure > 0 then
                Just <|
                    f <|
                        (Tuple.first event.pointer.clientPos
                            - rect.left
                        )
                            / rect.width

            else
                Nothing
        )
        Pointer.eventDecoder
        (Decode.field "pressure" Decode.float)
        (DOM.target DOM.boundingClientRect)
        |> Decode.andThen
            (\m ->
                case m of
                    Nothing ->
                        Decode.fail "no"

                    Just x ->
                        Decode.succeed x
            )
