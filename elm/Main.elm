module Main exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (attribute, class, height, id, property, src, width)
import Html.Events exposing (onClick)
import Json.Encode exposing (int, string)
import Task


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


type Msg
    = WindowResized Int Int


type alias Model =
    { screenSize : { width : Int, height : Int }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { width = 800, height = 600 } }
    , Task.perform
        (\{ viewport } ->
            WindowResized
                (round viewport.width)
                (round viewport.height)
        )
        getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized w h ->
            ( { model | screenSize = { width = w, height = h } }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    node "three-canvas"
        [ width model.screenSize.width
        , height model.screenSize.height
        , attribute "scene-id" "scene"
        ]
        [ node "three-scene"
            [ id "scene"
            , attribute "camera-id" "camera"
            ]
            [ node "camera-perspective"
                [ id "camera"
                , width model.screenSize.width
                , height model.screenSize.height
                ]
                []
            , node "three-mesh"
                []
                [ node "geometry-box" [] []
                , node "material-mesh-basic"
                    []
                    [ node "three-texture"
                        [ attribute "src" "../sample/1.png"
                        ]
                        []
                    ]
                ]
            ]
        ]


subs : Model -> Sub Msg
subs model =
    onResize WindowResized
