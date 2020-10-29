module CelAnimate.Editor.Timeline exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import DOM
import Dict
import Html exposing (Attribute, Html, div, input, label, node, span, text)
import Html.Attributes as Attr exposing (class, property, style, type_)
import Html.Events as Events exposing (on, onClick)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe
import Maybe.Extra as Maybe
import Round
import Set


type ParameterMsg
    = Use ParameterDesc Bool
    | SetValue ParameterDesc Float
    | MoveMarker ParameterDesc Float
    | SelectMarker Int


view : ParameterVector -> Selection -> Data -> Html Msg
view pv selection data =
    let
        mpart =
            selectedPart selection data

        paramNames =
            Maybe.map .parameters mpart
                |> Maybe.withDefault Dict.empty

        usingParams =
            Dict.values paramNames
                |> List.map (\p -> p.name)
                |> Set.fromList

        params =
            defaultParameters
                |> Array.mapToList (\p -> ( p, Set.member p.name usingParams ))

        markers desc =
            Maybe.unwrap Array.empty .keyframes mpart
                |> Array.map
                    (\keyframe ->
                        { value = getValue desc keyframe.vector
                        , projection =
                            parameterProjection paramNames
                                keyframe.vector
                                pv
                                desc
                        }
                    )

        message msg =
            case msg of
                Use desc use ->
                    ModifyData <| useParameter desc use

                SetValue desc value ->
                    Batch
                        (SelectData { selection | keyframe = -1 })
                        (ChangeParameters <| Dict.singleton desc.name value)

                MoveMarker desc value ->
                    Batch
                        (ChangeParameters <| Dict.singleton desc.name value)
                        (ModifyData <|
                            \s ->
                                updateKeyframe s <|
                                    \keyframe ->
                                        { keyframe
                                            | vector =
                                                Dict.insert desc.name value keyframe.vector
                                        }
                        )

                SelectMarker i ->
                    let
                        newSelection =
                            { selection | keyframe = i }

                        newPV =
                            Maybe.map2
                                (\part keyframe ->
                                    extractParameters part.parameters
                                        keyframe.vector
                                )
                                (selectedPart newSelection data)
                                (selectedKeyframe newSelection data)
                                |> Maybe.withDefault Dict.empty
                    in
                    Batch
                        (ChangeParameters newPV)
                        (SelectData newSelection)
    in
    Html.map message <|
        div [ class "h-1-4 bg-gray-800 " ] <|
            List.map
                (\( desc, using ) ->
                    let
                        value =
                            Dict.get desc.name pv
                                |> Maybe.withDefault (defaultValue desc)
                    in
                    parameter (Parameter desc value)
                        using
                        (markers desc)
                        selection.keyframe
                )
                params


type alias Marker =
    { value : Float
    , projection : Float
    }


parameter : Parameter -> Bool -> Array Marker -> Int -> Html ParameterMsg
parameter p using markers markerSelection =
    div [ class "p-px" ]
        [ div [ class "bg-gray-700 flex flex-row items-center" ]
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
            , slider using
                (minimumValue p.desc)
                (maximumValue p.desc)
                p.value
                markers
                markerSelection
                |> Html.map
                    (\msg ->
                        case msg of
                            Change value ->
                                SetValue p.desc value

                            Move value ->
                                MoveMarker p.desc value

                            Select i ->
                                SelectMarker i
                    )
            ]
        ]


onCheck : (Bool -> msg) -> Attribute msg
onCheck tagger =
    Events.preventDefaultOn "input" <|
        Decode.map (\checked -> ( tagger checked, True )) Events.targetChecked


type SliderMsg
    = Change Float
    | Move Float
    | Select Int


slider :
    Bool
    -> Float
    -> Float
    -> Float
    -> Array Marker
    -> Int
    -> Html SliderMsg
slider using min max value markers markerSelection =
    let
        headColor =
            if using then
                "bg-red-600"

            else
                "bg-gray-500"

        keyframeMarker i marker =
            node "slider-object"
                [ class """w-3 h-3 fixed transform relative -mr-3
                          border-solid border border-black hover:border-white"""
                , if i == markerSelection then
                    class "bg-teal-400 z-50"

                  else
                    class "bg-gray-300"
                , floatAttr "value" marker.value
                , style "top" <|
                    String.fromFloat (marker.projection * -50)
                        ++ "%"
                , style "transform" <|
                    "translate(-50%, 50%) rotate(45deg) scale("
                        ++ String.fromFloat (1 - abs marker.projection / 2)
                        ++ ")"
                , style "z-index" <|
                    String.fromInt <|
                        abs (round <| 40 - abs marker.projection * 40)
                , on "down" <| Decode.succeed <| Select i
                , Events.on "change" <|
                    Decode.map Move <|
                        targetValue Decode.float
                ]
                []
    in
    div [ class "flex flex-grow flex-shrink " ]
        [ node "slider-track"
            [ class "h-6 flex flex-auto bg-gray-800 m-1 relative"
            , floatAttr "value" value
            , floatAttr "min" min
            , floatAttr "max" max
            , Events.on "change" <|
                Decode.map Change <|
                    targetValue Decode.float
            ]
          <|
            node "slider-object"
                [ class <|
                    "h-6 w-px fixed pointer-events-none relative "
                        ++ headColor
                , floatAttr "value" value
                ]
                []
                :: (if using then
                        Array.indexedMapToList keyframeMarker markers

                    else
                        []
                   )
        , span [ class "w-24" ]
            [ text <| Round.round 2 value ]
        ]


useParameter : ParameterDesc -> Bool -> Selection -> Data -> Data
useParameter desc use selection data =
    let
        update part =
            { part
                | parameters =
                    if use then
                        Dict.insert desc.name desc part.parameters

                    else
                        Dict.remove desc.name part.parameters
            }
    in
    updatePart selection update data
