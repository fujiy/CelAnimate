module CelAnimate.Editor.Property exposing (view)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import File
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes as Attr exposing (class, src)
import Html.Events as Events exposing (onClick)
import Maybe
import Maybe.Extra as Maybe
import Tuple


view : Selection -> Data -> Html Msg
view selection data =
    div [ class "flex flex-col w-48 bg-gray-800 p-px" ]
        [ maybe partProperty <| selectedPart selection data
        , maybe (celProperty selection) <| selectedCel selection data
        ]


partProperty : Part -> Html Msg
partProperty part =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text part.name ]
        ]


celProperty : Path -> Cel -> Html Msg
celProperty path cel =
    div [ class "bg-gray-700 m-px p-2" ]
        [ p [] [ text cel.name ]
        , div [ class "bg-gray-700 m-px" ]
            [ icon_ "image"
            , button
                [ class "bg-gray-800"
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
            [ button
                [ class "bg-gray-800 hover:bg-gray-900 w-full my-1"
                , onClick <|
                    Batch
                        (ModifyData <|
                            \selection ->
                                updateCel selection MeshEdit.clearCel
                        )
                        (SwitchMode <|
                            MeshEdit.start cel
                        )
                ]
                [ text "Create Meshes" ]
            ]
        ]
