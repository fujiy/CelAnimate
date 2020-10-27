module CelAnimate.Editor.Property exposing (view)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import File
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Maybe
import Maybe.Extra as Maybe


view : Selection -> Data -> Html Msg
view selection data =
    div [ class "flex flex-col w-48 bg-gray-800 p-px" ]
        [ maybe celProperty <| selectedCel selection data
        , maybe keyframeProperty <| selectedKeyframe selection data
        ]


celProperty : Cel -> Html Msg
celProperty cel =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text cel.name ]
        ]


keyframeProperty : Keyframe -> Html Msg
keyframeProperty keyframe =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text keyframe.name ]
        , div [ class "bg-gray-700 m-px" ]
            [ icon_ "image"
            , button
                [ class "bg-gray-800"
                , onClick <| FileAction FileSelect
                ]
                [ maybe (text << File.name << Tuple.first) keyframe.image
                , icon_ "folder-open"
                ]
            ]
        , img [ src <| Maybe.unwrap "" Tuple.second <| keyframe.image] []
        , p []
            [ button
                [ class "bg-gray-800 hover:bg-gray-900 w-full my-1"
                , onClick <|
                    Batch
                        (ModifyData <|
                            \selection ->
                                updateKeyframe selection MeshEdit.clearKeyframe
                        )
                        (SwitchMode <|
                            MeshEdit.start keyframe
                        )
                ]
                [ text "Create Meshes" ]
            ]
        ]
