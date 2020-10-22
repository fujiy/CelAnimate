module CelAnimate.Editor exposing (..)

import Array
import Array.Extra as Array
import Array.More as Array
import Browser.Dom exposing (getViewport)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Editor.Outliner exposing (..)
import CelAnimate.Editor.Timeline exposing (..)
import CelAnimate.Editor.Viewport exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Dict
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Math.Vector3 as Vec3 exposing (Vec3)
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewportSize = { width = 800, height = 600 }
      , camera = initCameraState
      , toolState = initToolState
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
      , dataSelection = DataSelection 0 0
      , parameters = Dict.empty
      }
    , Cmd.none
      -- , Task.perform
      --     (\{ viewport } ->
      --         ViewportResized
      --             (round viewport.width)
      --             (round viewport.height)
      --     )
      -- getViewport
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
                            { model
                                | data = { data | cels = cels }
                                , toolState =
                                    PolygonDraw PolygonDraw.initState
                            }

                        Nothing ->
                            model

                _ ->
                    model

        PolygonErase state ->
            case msg of
                ToolStart ->
                    case currentKeyframe model of
                        Just keyframe ->
                            { model
                                | toolState =
                                    PolygonErase <|
                                        PolygonErase.start
                                            model.toolSettings.polygonErase
                                            tool
                                            keyframe
                            }

                        Nothing ->
                            model

                ToolMove ->
                    { model
                        | toolState =
                            PolygonErase <|
                                PolygonErase.step
                                    model.toolSettings.polygonErase
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
                                    PolygonErase.finish state keyframe

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
                            { model
                                | data = { data | cels = cels }
                                , toolState =
                                    PolygonErase PolygonErase.initState
                            }

                        Nothing ->
                            model

                _ ->
                    model

        PolygonMove state ->
            case msg of
                ToolStart ->
                    case currentKeyframe model of
                        Just keyframe ->
                            { model
                                | toolState =
                                    PolygonMove <|
                                        PolygonMove.start
                                            model.toolSettings.polygonMove
                                            tool
                                            keyframe
                            }

                        Nothing ->
                            model

                ToolMove ->
                    { model
                        | toolState =
                            PolygonMove <|
                                PolygonMove.step
                                    model.toolSettings.polygonMove
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
                                    PolygonMove.finish state keyframe

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
                            { model
                                | data = { data | cels = cels }
                                , toolState =
                                    PolygonMove PolygonMove.initState
                            }

                        Nothing ->
                            model

                _ ->
                    model


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToolInput msg tool ->
            ( toolInput model msg tool
            , Cmd.none
            )

        ToolChange state ->
            ( { model | toolState = state }
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

        Parameters msg ->
            case msg of
                SetValue o ->
                    ( { model
                        | parameters =
                            Dict.insert o.name o.value model.parameters
                      }
                    , Cmd.none
                    )

                ParameterUse o ->
                    let
                        updateCel cel =
                            { cel
                                | parameters =
                                    if o.use then
                                        Dict.insert o.desc.name
                                            o.desc
                                            cel.parameters

                                    else
                                        Dict.remove o.desc.name cel.parameters
                            }
                    in
                    ( updateCurrentCel updateCel model
                    , Cmd.none
                    )

        ViewportResized w h ->
            let
                camera =
                    model.camera
            in
            ( { model
                | viewportSize = { width = w, height = h }
                , camera = { camera | aspect = toFloat w / toFloat h }
              }
            , Cmd.none
            )

        CanvasPointer msg ->
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
                    case msg of
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

                toolMsg =
                    case msg of
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
            , Task.perform identity <| Task.succeed <| ToolInput toolMsg tool
            )


view : Model -> Html Msg
view model =
    div [ class "flex flex-row w-screen text-white select-none" ]
        [ toolBar model
        , div [ class "flex flex-col flex-grow flex-shrink" ]
            [ div [ class "flex flex-row flex-grow flex-shrink" ]
                [ dataTree model.data model.dataSelection.cel
                , viewport model
                ]
            , timeline model.parameters <| currentCel model
            ]
        ]


toolBar : Model -> Html Msg
toolBar model =
    let
        tool =
            case model.toolState of
                PolygonMove _ ->
                    0

                PolygonDraw _ ->
                    1

                PolygonErase _ ->
                    2
    in
    div
        [ class <|
            "h-screen w-8 bg-gray-700 flex flex-col text-xl select-none "
                ++ "pointer-events-auto flex-grow-0 flex-shrink-0"
        ]
        [ toolIcon "arrows-alt" (tool == 0) <|
            PolygonMove PolygonMove.initState
        , toolIcon "paint-brush" (tool == 1) <|
            PolygonDraw PolygonDraw.initState
        , toolIcon "eraser" (tool == 2) <|
            PolygonErase PolygonErase.initState
        ]


toolIcon : String -> Bool -> ToolState -> Html Msg
toolIcon name now state =
    button
        [ class <|
            "p-1 select-none focus:outline-none "
                ++ (if now then
                        "bg-teal-700"

                    else
                        ""
                   )
        , onClick <| ToolChange state
        ]
        [ icon name ]


cursorView : Model -> Html Msg
cursorView model =
    let
        ( x, y ) =
            model.cursor.position

        -- diameter = model.toolSettings.polygonDraw.radius * 2
    in
    div
        [ class "cursor-polygon-draw fixed"
        , style "top" (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        , style "width" "20px"
        , style "height" "20px"
        ]
        []


subs : Model -> Sub Msg
subs model =
    Sub.none
