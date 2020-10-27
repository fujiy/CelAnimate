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


type alias Scene msg =
    Three msg


view : Model -> Html Msg
view model =
    node "three-canvas"
        [ attribute "scene-id" "model-scene tool-scene"
        , attribute "camera-id" "camera"
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
        [ camera model.camera
        , modelScene model
        , toolScene model
        ]


modelScene : Model -> Scene msg
modelScene model =
    node "three-scene"
        [ id "model-scene"
        , attribute "background" "#1A202C"
        ]
        [ case model.mode of
              MorphMode ->
               body model.selection model.parameters model.data.cels
              MeshEditMode state using mesh ->
                  selectedKeyframe model.selection model.data |>
                  maybe (\keyframe -> editingKeyframe keyframe mesh)
        ]


toolScene : Model -> Scene msg
toolScene model =
    node "three-scene"
        [ id "tool-scene" ]
        [ toolView model.mode <| selectedKeyframe model.selection model.data
        , cursorObject model
        ]


editingKeyframe : Keyframe -> Mesh -> Three msg
editingKeyframe keyframe mesh =
    imagePlane keyframe.image

imagePlane : Image -> Three msg
imagePlane image =
    if isLoaded image then
        node "three-mesh"
            []
            [ node "geometry-plane"
                  [ floatAttr "width" <| Tuple.first image.size / image.ppm
                  , floatAttr "height" <| Tuple.second image.size / image.ppm
                  ]
                  []
            , node "material-mesh-basic"
                [ boolAttr "transparent" True
                , floatAttr "opacity" 0.5
                ]
                  [ node "three-texture" [ src image.src ] [] ]
            ]
    else text ""

toolView : ModeState -> Maybe Keyframe -> Three msg
toolView mode mk =
    node "three-group"
        []
        [ case mode of
            MorphMode ->
                text ""

            MeshEditMode state using mesh ->
                if using then
                    maybe (\keyframe -> 
                           meshObject True keyframe.image <|
                               MeshEdit.progress keyframe.image state)
                    mk

                else
                    maybe (\keyframe -> meshObject True keyframe.image mesh) mk
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
    maybe (\keyframe -> meshObject False keyframe.image keyframe.mesh)
        (Array.get selection.keyframe cel.keyframes)


meshObject : Bool -> Image -> Mesh -> Three msg
meshObject showMesh image mesh =
    let
        vertices =
            Encode.array encodeVec3 mesh.vertices

        faces =
            Encode.array encodeFace mesh.faces

        uvs =
            Encode.array encodeUVVec mesh.mapping
    in
    Html.node "three-group"
        []
        [ if showMesh then
            Html.node "three-mesh"
                [ position <| vec3 0 0 0.01 ]
                [ Html.node "geometry-buffer"
                    [ property "vertices" vertices
                    , property "faces" faces
                    ]
                    []
                , Html.node "material-mesh-basic"
                    [ attribute "color" "#E53E3E"
                    , boolAttr "wireframe" True
                    ]
                    []
                ]

          else
            text ""
        , Html.node "three-mesh"
            []
            [ Html.node "geometry-buffer"
                [ property "vertices" vertices
                , property "faces" faces
                , property "uvs" uvs
                ]
                []
            ,  if isLoaded image then
                   Html.node "material-mesh-basic"
                       [ boolAttr "transparent" True]
                  [ node "three-texture" [ src image.src ] [] ]
               else
                   Html.node "material-mesh-basic"
                [ attribute "color" "white"
                , boolAttr "transparent" True
                , floatAttr "opacity" 0.5
                ]
                []
            ]
        ]

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

