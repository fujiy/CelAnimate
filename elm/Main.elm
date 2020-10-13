module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor exposing (..)
import CelAnimate.Tool exposing (..)
import CelAnimate.Tool.PolygonDraw exposing (..)
import Html exposing (Attribute, Html, button, div, node, text)
import Html.Attributes exposing (attribute, class, height, id, property, src, style, width)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Json.Encode as Json exposing (int, list, string)
import KdTree
import Math.Vector3 as Vec3 exposing (Vec3)
import String
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
    | CanvasPointer CanvasPointerMsg


type CanvasPointerMsg
    = PointerDown
    | PointerMove ( Float, Float )
    | PointerUp


type alias Model =
    { screenSize :
        { width : Int
        , height : Int
        }
    , camera : CameraState
    , toolSettings : ToolSettings
    , toolState : ToolState
    , cursor : Cursor
    , data : Data
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { width = 800, height = 600 }
      , camera = initCameraState
      , toolState = initToolState
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
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

                toolState =
                    model.toolState

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
                                        drawPolygons model.toolSettings
                                            tool
                                            initPolygonDrawState

                                PointerMove _ ->
                                    if model.cursor.down then
                                        PolygonDraw <|
                                            drawPolygons model.toolSettings
                                                tool
                                                state

                                    else
                                        model.toolState

                                PointerUp ->
                                    PolygonDraw <|
                                        initPolygonDrawState

                -- cmd =
                --     case newToolState of
                --         PolygonDraw ts ->
                --             Debug.log
                --                 (Debug.toString <|
                --                     KdTree.toArray ts.kdTree
                --                 )
                --                 ()
            in
            ( { model
                | cursor = newCursor
                , toolState = newToolState
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ cursorView model
        , node "three-canvas"
            [ width model.screenSize.width
            , height model.screenSize.height
            , attribute "scene-id" "scene"
            , Pointer.onMove
                (\event ->
                    CanvasPointer <|
                        PointerMove event.pointer.offsetPos
                )
            , Pointer.onDown (\_ -> CanvasPointer PointerDown)
            , Pointer.onUp (\_ -> CanvasPointer PointerUp)
            , Pointer.onCancel (\_ -> CanvasPointer PointerUp)
            ]
            [ node "three-scene"
                [ id "scene"
                , attribute "camera-id" "camera"
                ]
                [ node "camera-perspective"
                    [ id "camera"
                    , floatAttr "aspect" model.camera.aspect
                    , floatAttr "fov" model.camera.fov
                    ]
                    []
                , case model.toolState of
                    PolygonDraw state ->
                        polygonMesh <| drawingPolygon state

                -- , node "three-mesh"
                --     []
                -- [ node "geometry-buffer"
                --     (case model.toolState of
                --         PolygonDraw state ->
                --             [ property "vertices" <|
                --                 Json.list (\( _, v ) -> encodeVec3 v) <|
                --                     List.sortBy (\( i, _ ) -> i) <|
                --                         Array.toList <|
                --                             KdTree.toArray state.kdTree
                --             , property "faces" <|
                --                 Json.array encodeFace <|
                --                     state.polygons
                --             ]
                --     )
                --     []
                -- , node "material-mesh-basic"
                --     [ attribute "color" "black"
                --     , boolAttr "wireframe" True
                --     ]
                --     []
                -- ]
                , node "three-line-segments"
                    [ property "position" <|
                        encodeVec3 <|
                            cursorPosition model model.cursor.position
                    ]
                    [ node "geometry-edges"
                        []
                        [ node "geometry-circle"
                            [ floatAttr "radius"
                                model.toolSettings.polygonDraw.radius
                            , intAttr "segments" 16
                            ]
                            []
                        ]
                    , node "material-line-basic" [ attribute "color" "red" ] []
                    ]

                -- , node "three-mesh"
                --     []
                --     [ node "geometry-box" [] []
                --     , node "material-mesh-basic"
                --         []
                --         [ node "three-texture"
                --             [ attribute "src" "../sample/1.png"
                --             ]
                --             []
                --         ]
                --     ]
                ]
            ]
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


cursorPosition : Model -> ( Float, Float ) -> Vec3
cursorPosition model ( cx, cy ) =
    let
        size =
            viewSize model.camera (Vec3.length model.camera.position)

        ( w, h ) =
            ( toFloat model.screenSize.width
            , toFloat model.screenSize.height
            )

        -- ( cx, cy ) =
        --     cursor.position
        x =
            (cx - w / 2) / w * size.width / 2

        y =
            (h / 2 - cy) / h * size.height / 2
    in
    Vec3.vec3 x y 0


cursorVelocity : Model -> ( Float, Float ) -> Vec3
cursorVelocity model ( vx, vy ) =
    let
        size =
            viewSize model.camera (Vec3.length model.camera.position)

        ( w, h ) =
            ( toFloat model.screenSize.width
            , toFloat model.screenSize.height
            )

        -- ( cx, cy ) =
        --     cursor.position
        x =
            vx / w * size.width / 2

        y =
            (0 - vy) / h * size.height / 2
    in
    Vec3.vec3 vx (0 - vy) 0



-- cursorVelocity : Model -> Cursor -> Vec3
-- cursorVelocity model cursor =
--     let newV =
--             Vec3.sub (cursorPosition model cursor)
--                 (cursorPosition model model.cursor)
--     in Vec3.add (Vec3.scale 0.5 newV) (Vec3.scale 0.5 model.cursor.)


subs : Model -> Sub Msg
subs model =
    onResize WindowResized
