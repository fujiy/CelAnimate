module CelAnimate.Editor exposing (..)

import Array
import Array.Extra as Array
import Array.More as Array
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Editor.Outliner exposing (..)
import CelAnimate.Editor.Viewport exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Math.Vector3 as Vec3 exposing (Vec3)
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { width = 800, height = 600 }
      , camera = initCameraState
      , toolState = initToolState
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
      , dataSelection = 0
      }
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
        DataTree dt ->
            case dt of
                SelectCel i ->
                    ( { model | dataSelection = i }
                    , Cmd.none
                    )

                NewCel i ->
                    let
                        data =
                            model.data
                    in
                    ( { model
                        | data =
                            { data
                                | cels = Array.insertAt i zeroCel data.cels
                            }
                      }
                    , Cmd.none
                    )

        WindowResized w h ->
            let
                camera =
                    model.camera
            in
            ( { model
                | screenSize = { width = w, height = h }
                , camera = { camera | aspect = toFloat w / toFloat h }
              }
            , Cmd.none
            )

        CanvasPointer cmsg ->
            let
                cursor =
                    model.cursor

                ( vx, vy ) =
                    cursor.velocity

                velocity pos =
                    ( (Tuple.first pos - Tuple.first cursor.position)
                        * 0.5
                        + vx
                        * 0.5
                    , (Tuple.second pos - Tuple.second cursor.position)
                        * 0.5
                        + vy
                        * 0.5
                    )

                newCursor =
                    case cmsg of
                        PointerDown ->
                            { cursor
                                | down = True
                            }

                        PointerUp ->
                            { cursor
                                | down = False
                            }

                        PointerMove pos ->
                            { cursor
                                | position = pos
                                , velocity = velocity pos
                            }

                tool =
                    { center = cursorPosition model newCursor.position
                    , direction = cursorVelocity model newCursor.velocity
                    , u = Vec3.vec3 1 0 0
                    , v = Vec3.vec3 0 1 0
                    }

                newToolState =
                    case model.toolState of
                        PolygonDraw state ->
                            case cmsg of
                                PointerDown ->
                                    PolygonDraw <|
                                        PolygonDraw.drawPolygons
                                            model.toolSettings.polygonDraw
                                            tool
                                            PolygonDraw.initState

                                PointerMove _ ->
                                    if model.cursor.down then
                                        PolygonDraw <|
                                            PolygonDraw.drawPolygons
                                                model.toolSettings.polygonDraw
                                                tool
                                                state

                                    else
                                        model.toolState

                                PointerUp ->
                                    PolygonDraw <|
                                        PolygonDraw.initState
            in
            ( { model
                | cursor = newCursor
                , toolState = newToolState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "text-white" ]
        [ dataTree model.data model.dataSelection
        , cursorView model
        , viewport model
        ]


cursorView : Model -> Html Msg
cursorView model =
    let
        ( x, y ) =
            model.cursor.position

        -- diameter = model.toolSettings.polygonDraw.radius * 2
    in
    div
        [ class "cursor-polygon-draw"
        , style "top" (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        , style "width" "20px"
        , style "height" "20px"
        ]
        []


subs : Model -> Sub Msg
subs model =
    onResize WindowResized
