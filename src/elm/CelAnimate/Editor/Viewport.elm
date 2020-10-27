module CelAnimate.Editor.Viewport exposing (view)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Dict
import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute, class, id, property, src)
import Html.Events.Extra.Pointer as Pointer
import Json.Encode as Encode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe


view : Model -> Html Msg
view model =
    node "three-canvas"
        [ attribute "scene-id" "scene"
        , boolAttr "auto-size" True
        , class "flex flex-auto"
        , Pointer.onMove <| Pointer PointerMove
        , Pointer.onDown <| Pointer PointerDown
        , Pointer.onUp <| Pointer PointerUp
        , Pointer.onCancel <| Pointer PointerCancel
        , onResize
            (\rect ->
                ViewportResized (round rect.width) (round rect.height)
            )
        ]
        [ node "three-scene"
            [ id "scene"
            , attribute "camera-id" "camera"
            , attribute "background" "#1A202C"
            ]
            [ camera model.camera
            , body model.selection model.parameters model.data.cels
            , toolView model.mode model.selection model.data
            , cursorObject model
            ]
        ]



toolView : ModeState -> Selection -> Data -> Three msg
toolView mode selection data =
    node "three-group" []
        [case Maybe.andThen .image <| selectedKeyframe selection data of
              Nothing -> text ""
              Just (_, url) ->
                  node "three-mesh"
                      []
                      [ node "geometry-plane"
                            [floatAttr "width" 2
                            , floatAttr "height" 2] []
                      , node "material-mesh-basic"
                          [boolAttr "transparent" True]
                          [node "three-texture" [src url] []]
                      ]
        , case mode of
             MorphMode -> text ""
             MeshEditMode state using mesh ->
                             if using then
                                 polygonMesh True <| MeshEdit.progress state
                             else
                                 polygonMesh False mesh
        ]

body : Selection -> ParameterVector -> Array Cel -> Three msg
body selection pv cels =
    let
        pitch =
            Dict.get "pitch" pv |> Maybe.unwrap 0 degrees

        yaw =
            Dict.get "yaw" pv |> Maybe.unwrap 0 degrees

        roll =
            Dict.get "roll" pv |> Maybe.unwrap 0 degrees
    in
    node "three-group"
        [ rotation <| vec3 pitch yaw roll ]
    <|
        List.append
            [ node "axes-helper" [] [] ]
        <|
            Array.mapToList (celObject selection) cels


celObject : Selection -> Cel -> Three msg
celObject selection cel =
    maybe (\keyframe -> polygonMesh False keyframe.mesh)
        (Array.get selection.keyframe cel.keyframes)


camera : CameraState -> Three msg
camera state =
    node "camera-perspective"
        [ id "camera"
        , boolAttr "auto-aspect" True
        , floatAttr "fov" state.fov
        , position state.position
        , lookAt state.lookAt
        ]
        []


cursorObject : Model -> Three msg
cursorObject model =
    node "three-line-segments"
        [ position <|
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
