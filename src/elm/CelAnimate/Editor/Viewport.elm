module CelAnimate.Editor.Viewport exposing (..)

import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute, height, id, property, width)
import Html.Events.Extra.Pointer as Pointer
import Json.Encode as Encode
import Math.Vector3 as Vec3 exposing (Vec3)


viewport : Model -> Html Msg
viewport model =
    node "three-canvas"
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
            , attribute "background" "#1A202C"
            ]
          <|
            List.append
                [ node "camera-perspective"
                    [ id "camera"
                    , floatAttr "aspect" model.camera.aspect
                    , floatAttr "fov" model.camera.fov
                    ]
                    []
                , case model.toolState of
                    PolygonDraw state ->
                        polygonMesh True <|
                            PolygonDraw.progress state

                    PolygonErase state ->
                        polygonMesh True <|
                            PolygonErase.progress state

                    PolygonMove state ->
                        polygonMesh True <|
                            PolygonMove.progress state
                , cursor model
                ]
                (case currentKeyframe model of
                    Just keyframe ->
                        let
                            drawing =
                                case model.toolState of
                                    PolygonDraw state ->
                                        state.drawing

                                    PolygonErase state ->
                                        state.using

                                    PolygonMove state ->
                                        state.using
                        in
                        if drawing then
                            []

                        else
                            [ polygonMesh False keyframe.mesh ]

                    Nothing ->
                        []
                )
        ]


cursor : Model -> Three msg
cursor model =
    node "three-line-segments"
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
        , node "material-line-basic" [ attribute "color" "white" ] []
        ]


polygonMesh : Bool -> Mesh -> Three msg
polygonMesh colored mesh =
    let
        vertices =
            Encode.array encodeVec3 mesh.vertices

        faces =
            Encode.array encodeFace mesh.faces
    in
    Html.node "three-group"
        []
        [ Html.node "three-mesh"
            []
            [ Html.node "geometry-buffer"
                [ property "vertices" vertices
                , property "faces" faces
                ]
                []
            , Html.node "material-mesh-basic"
                [ attribute "color" "black"
                , boolAttr "wireframe" True
                ]
                []
            ]
        , Html.node "three-mesh"
            []
            [ Html.node "geometry-buffer"
                [ property "vertices" vertices
                , property "faces" faces
                ]
                []
            , Html.node "material-mesh-basic"
                [ attribute "color"
                    (if colored then
                        "#4FD1C5"

                     else
                        "white"
                    )
                , boolAttr "wireframe" False
                , boolAttr "transparent" True
                , floatAttr "opacity"
                    (if colored then
                        0.5

                     else
                        0.2
                    )
                ]
                []
            ]
        ]


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
