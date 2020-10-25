module CelAnimate.Editor.Property exposing (view)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import File
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Maybe
import Maybe.Extra as Maybe


view : DataSelection -> Data -> Html Msg
view selection data =
    div [ class "flex w-48 bg-gray-800 p-px" ]
        [ keyframeImage <| selectedKeyframe selection data ]


keyframeImage : Maybe Keyframe -> Html Msg
keyframeImage keyframe =
    div []
        [ div [ class "bg-gray-700 m-px" ]
            [ button
                [ class "select-none outline-none"
                , onClick <| FileAction FileSelect
                ]
                [ icon "folder-open" ]
            , text <|
                Maybe.unwrap "" (Tuple.first >> File.name) <|
                    Maybe.andThen .image keyframe
            ]
        , img
            [ src <|
                Maybe.unwrap "" Tuple.second <|
                    Maybe.andThen .image keyframe
            ]
            []
        ]
