module Main exposing (..)

import Browser
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (attribute, class, height, id, property, width)
import Html.Events exposing (onClick)
import Json.Encode exposing (int, string)


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : number -> Html Msg
view model =
    node "three-canvas"
        [ width 400
        , height 300
        , attribute "scene-id" "scene"
        ]
        [ node "three-scene"
            [ id "scene"
            , attribute "camera-id" "camera"
            ]
            [ node "camera-perspective"
                [ id "camera"
                , width 400
                , height 300
                ]
                []
            , node "three-mesh"
                []
                [ node "geometry-box" [] []
                , node "material-mesh-basic"
                    [ attribute "color" "#00ff00"
                    ]
                    []
                ]
            ]
        ]
