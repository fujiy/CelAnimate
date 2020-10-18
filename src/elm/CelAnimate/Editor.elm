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
      , dataSelection = DataSelection 0 0
      }
    , Task.perform
        (\{ viewport } ->
            WindowResized
                (round viewport.width)
                (round viewport.height)
        )
        getViewport
    )


toolInput : Model -> ToolMsg -> Tool -> Model
toolInput model msg tool =
    case model.toolState of
        PolygonDraw state ->
            case msg of
                ToolStart ->
                    case currentKeyframe model of
                        Just keyframe ->
                            { model
                                | toolState =
                                    PolygonDraw <|
                                        PolygonDraw.start
                                            model.toolSettings.polygonDraw
                                            tool
                                            keyframe
                            }

                        Nothing ->
                            model

                ToolMove ->
                    { model
                        | toolState =
                            PolygonDraw <|
                                PolygonDraw.step
                                    model.toolSettings.polygonDraw
                                    tool
                                    state
                    }

                ToolFinish ->
                    case currentKeyframe model of
                        Just keyframe ->
                            let
                                data =
                                    model.data

                                selection =
                                    model.dataSelection

                                newKeyframe =
                                    PolygonDraw.finish state keyframe

                                updateKeyframe cel =
                                    { cel
                                        | keyframes =
                                            Array.set selection.keyframe
                                                newKeyframe
                                                cel.keyframes
                                    }

                                cels =
                                    Array.update selection.cel
                                        updateKeyframe
                                        data.cels
                            in
                            Debug.log "model"
                                { model
                                    | data = { data | cels = cels }
                                    , toolState =
                                        PolygonDraw PolygonDraw.initState
                                }

                        Nothing ->
                            model

                _ ->
                    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToolInput tmsg tool ->
            ( toolInput model tmsg tool
            , Cmd.none
            )

        DataTree dt ->
            case dt of
                SelectCel i ->
                    let
                        selection =
                            model.dataSelection
                    in
                    ( { model
                        | dataSelection = { selection | cel = i }
                      }
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

                message =
                    case cmsg of
                        PointerDown ->
                            ToolStart

                        PointerUp ->
                            ToolFinish

                        PointerMove _ ->
                            if newCursor.down then
                                ToolMove

                            else
                                ToolHover
            in
            ( { model
                | cursor = newCursor
              }
            , Task.perform identity <| Task.succeed <| ToolInput message tool
            )


view : Model -> Html Msg
view model =
    div [ class "text-white" ]
        [ dataTree model.data model.dataSelection.cel
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
