module CelAnimate.Editor.Property exposing (view)

import Array
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import Dict
import File
import Html exposing (Html, div, img, p, span, text, input, label, node)
import Html.Attributes as Attr exposing (class, disabled, src, type_, value)
import Html.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode
import Maybe
import Maybe.Extra as Maybe
import Round
import Tuple


view : ParameterVector -> Selection -> Data -> Html Msg
view pv selection data =
    div [ class "flex flex-col w-48 bg-gray-800 p-px overflow-y-scroll" ]
        [ maybe partProperties <| selectedPart selection data
        , maybe
            (celProperties pv selection <| selectedKeyframe selection data)
          <|
            selectedCel selection data
        , keyframeProperties pv selection data
        ]


partProperties : Part -> Html Msg
partProperties part =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text "Part: ", text part.name ]
        , p []
            [ button "Calcurate interpolation" <|
                ModifyData calcInterpolationOfSelectedPart
            ]
        ]


celProperties : ParameterVector -> Path -> Maybe Keyframe -> Cel -> Html Msg
celProperties pv path mkeyframe cel =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text "Cel: ", text cel.name ]
        , div [ class "bg-gray-700 m-px" ]
            [ icon_ "image"
            , Html.button
                [ class "bg-gray-800 hover:bg-gray-900"
                , onClick <| FileAction ImageSelect
                ]
                [ text cel.image.name
                , icon_ "folder-open"
                ]
            ]
        , img
            [ src <| cel.image.uri
            , Attr.map (FileAction << GotImageSize path) <|
                Events.on "load" targetImgSize
            ]
            []
        , p []
            [ text <| String.fromFloat <| Tuple.first cel.image.size
            , text "px × "
            , text <| String.fromFloat <| Tuple.second cel.image.size
            , text "px"
            ]
        , p [class "my-1"]
            [ input
                  [type_ "number"
                  , class "w-16 bg-gray-800 outline-none"
                  , value <| String.fromFloat cel.image.ppm
                  , onInput <| \val ->
                      ModifyData <| \sel ->
                          case String.toFloat val of
                              Just ppm -> 
                                  updateCel sel <| setPPM ppm
                              Nothing -> identity
                  ] []
            , text " pixel per meter"
            ]
        , if isEmptyMesh cel.mesh then
            p []
                [ button "Create meshes" <|
                    Batch
                        (ModifyData <|
                            \selection ->
                                updateCel selection MeshEdit.clearCel
                        )
                        (SwitchMode <|
                            MeshEdit.start cel
                        )
                ]

          else
            ifThen
                (path.keyframe
                    /= -1
                    && not (Maybe.unwrap False (hasKeyCel cel.name) mkeyframe)
                )
            <|
                p []
                    [ button "Use in the keyframe" <|
                        ModifyData <|
                            \selection ->
                                updateKeyframe selection <| newKeyCel pv cel
                    ]
        ]


keyframeProperties : ParameterVector -> Selection -> Data -> Html Msg
keyframeProperties pv selection data =
    maybe
        (\part ->
            div [ class "bg-gray-700 m-px p-2" ] <|
                case selectedKeyframe selection data of
                    Nothing ->
                        [ Html.button
                            [ class "bg-gray-800 hover:bg-gray-900 w-full"
                            , disabled <| Dict.isEmpty part.parameters
                            , onClick <|
                                Batch
                                    (ModifyData <|
                                        newKeyframe pv <|
                                            Maybe.toList <|
                                                selectedCel selection data
                                    )
                                    (SelectData
                                        { selection
                                            | keyframe =
                                                Array.length part.keyframes
                                        }
                                    )
                            ]
                            [ text "Add Keyframe" ]
                        ]

                    Just keyframe ->
                        [ p [] [ text "Keyframe: ", text keyframe.name ]
                        , div [] <| List.map keyCelProperties keyframe.cels
                        , node "context-menu"
                            [ class "bg-gray-700 shadow-xl w-32 "
                            ]
                              [ p
                                [ class "hover:bg-gray-800 p-1"
                                , onClick <| ModifyData deleteKeyframe
                                ]
                                [ text "Delete Keyframe" ]
                              ]
                        ]
        )
    <|
        selectedPart selection data


keyCelProperties : KeyCel -> Html Msg
keyCelProperties keycel =
    div [ class "m-1 border-gray-800 border-t" ]
        [ p [] [ text keycel.name ]
        , p []
            [ label []
                  [ text "show"
                  , checkbox keycel.show
                  |> Html.map
                       (\check ->
                            ModifyData <|
                            flip updateKeyCel (\k -> { k | show = check}))
                  ]
            ]
        , p []
            [ text "opacity: "
            , text <| Round.round 2 keycel.opacity
            , slider 0 1 0.01 2 keycel.opacity
                |> Html.map
                   (\v ->
                        ModifyData <|
                        flip updateKeyCel (\k -> { k | opacity = v}))
            ]
         , p []
            [ text "z: "
            , text <| Round.round 3 keycel.z
            , slider -0.1 0.1 0.001 3 keycel.z
            |> Html.map
                   (\v ->
                        ModifyData <|
                        flip updateKeyCel (\k -> { k | z = v}))
            ]
        ]
