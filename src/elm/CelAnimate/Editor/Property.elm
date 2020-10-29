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
import Html exposing (Html, div, img, p, span, text)
import Html.Attributes as Attr exposing (class, src, disabled)
import Html.Events as Events exposing (onClick)
import Maybe
import Maybe.Extra as Maybe
import Tuple


view : ParameterVector -> Selection -> Data -> Html Msg
view pv selection data =
    div [ class "flex flex-col w-48 bg-gray-800 p-px" ]
        [ maybe partProperties <| selectedPart selection data
        , maybe (celProperties selection) <| selectedCel selection data
        , keyframeProperties pv selection data
        ]


partProperties : Part -> Html Msg
partProperties part =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text "Part: ", text part.name ]
        ]


celProperties : Path -> Cel -> Html Msg
celProperties path cel =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text "Cel: ", text cel.name ]
        , div [ class "bg-gray-700 m-px" ]
            [ icon_ "image"
            , Html.button
                [ class "bg-gray-800 hover:bg-gray-900"
                , onClick <| FileAction FileSelect
                ]
                [ text <| imageName cel.image
                , icon_ "folder-open"
                ]
            ]
        , img
            [ src <| cel.image.src
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
        , p []
            [ button "Create Meshes" <|
                Batch
                    (ModifyData <|
                        \selection ->
                            updateCel selection MeshEdit.clearCel
                    )
                    (SwitchMode <|
                        MeshEdit.start cel
                    )
            ]
        ]


keyframeProperties : ParameterVector -> Selection -> Data -> Html Msg
keyframeProperties pv selection data =
    maybe (\part -> 
    div [ class "bg-gray-700 m-px p-2" ] <|
        case selectedKeyframe selection data of
            Nothing ->
                [ Html.button
                  [ class "bg-gray-800 hover:bg-gray-900 w-full"
                   , disabled <| Dict.isEmpty part.parameters
                  , onClick <| Batch
                        (ModifyData <|
                            newKeyframe pv <|
                                Maybe.toList <|
                                    selectedCel selection data
                        )
                        (SelectData
                            { selection
                                | keyframe = Array.length part.keyframes
                            }
                        )
                          ]
                     [text "Add Keyframe"]
                ]

            Just keyframe ->
                [ p [] [ text "Keyframe: ", text keyframe.name ] ]
          ) <| selectedPart selection data
