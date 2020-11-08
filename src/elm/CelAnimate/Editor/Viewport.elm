module CelAnimate.Editor.Viewport exposing (view)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Data.Encode as Encode
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import CelAnimate.Mode.Morph as Morph
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Dict
import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute, class, id, property, src)
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy exposing (..)
import Json.Encode as Encode
import List.Extra as List
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Tuple


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
            MorphMode state using ->
                body
                    model.mode
                    model.parameters
                    model.selection
                    model.data

            MeshEditMode state using mesh ->
                selectedCel model.selection model.data
                    |> maybe (\cel -> editingCel cel mesh)
        ]


toolScene : Model -> Scene msg
toolScene model =
    node "three-scene"
        [ id "tool-scene" ]
        [ toolView model.parameters
            model.mode
            (selectedCel model.selection model.data)
            (selectedKeyframe model.selection model.data)
            (selectedKeyCel model.selection model.data)
        , cursorObject model
        ]


editingCel : Cel -> Mesh -> Three msg
editingCel cel mesh =
    imagePlane cel.image


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
                [ node "three-texture" [ src image.uri ] [] ]
            ]

    else
        text ""


toolView :
    ParameterVector
    -> ModeState
    -> Maybe Cel
    -> Maybe Keyframe
    -> Maybe KeyCel
    -> Three msg
toolView pv mode mcel mkeyframe mkeycel =
    node "three-group"
        []
    <|
        case mode of
            MorphMode state using ->
                [ maybe_ <|
                    Maybe.map3
                        (\cel keyframe keycel ->
                            meshWireframe cel.mesh <|
                                if using then
                                    { keycel | morph = Morph.progress state }

                                else
                                    keycel
                        )
                        mcel
                        mkeyframe
                        mkeycel
                , node "axes-helper" [ rotation <| rotationEuler pv ] []
                ]

            MeshEditMode state using mesh ->
                let
                    editingMesh cel =
                        if using then
                            MeshEdit.progress cel.image state

                        else
                            mesh
                in
                Maybe.unwrap
                    []
                    (\cel ->
                        [ meshObject
                            cel.image
                            (editingMesh cel)
                            zeroKeyCel
                        , meshWireframe
                            (editingMesh cel)
                            zeroKeyCel
                        ]
                    )
                    mcel


body : ModeState -> ParameterVector -> Selection -> Data -> Three msg
body =
    lazy4 body_


body_ : ModeState -> ParameterVector -> Selection -> Data -> Three msg
body_ mode pv selection data =
    node "three-group" [] <|
        List.concat <|
            forParts data <|
                partObject mode pv selection


partObject :
    ModeState
    -> ParameterVector
    -> Selection
    -> Path
    -> Part
    -> List (Three msg)
partObject mode pv selection path part =
    forCels path
        part
        (\p cel ->
            let
                ip =
                    interpolate cel.interpolation pv

                keycel =
                    case mode of
                        MorphMode state using ->
                            if matchCel p selection then
                                if using then
                                    { ip | morph = Morph.progress state }

                                else
                                    Array.get selection.keyframe part.keyframes
                                        |> Maybe.andThen (keyCelMatches cel)
                                        |> Maybe.withDefault ip

                            else
                                ip

                        _ ->
                            ip
            in
            meshObject cel.image cel.mesh keycel
        )


meshObject : Image -> Mesh -> KeyCel -> Three msg
meshObject =
    lazy3 meshObject_


meshObject_ : Image -> Mesh -> KeyCel -> Three msg
meshObject_ image mesh key =
    let
        vertices =
            Encode.array Encode.vec3 <|
                addMorph key.morph key.z mesh.vertices

        faces =
            Encode.array encodeFace mesh.faces

        uvs =
            Encode.array encodeUVVec mesh.mapping
    in
    Html.node "three-mesh"
        [ floatAttr "render-order" key.z
        ]
        [ Html.node "geometry-buffer"
            [ property "vertices" vertices
            , property "faces" faces
            , property "uvs" uvs
            ]
            []
        , if isLoaded image then
            Html.node "material-mesh-basic"
                [ boolAttr "transparent" True
                , boolAttr "depth-test" False
                , floatAttr "opacity" <|
                    if key.show then
                        key.opacity

                    else
                        0
                ]
                [ node "three-texture" [ src image.uri ] [] ]

          else
            Html.node "material-mesh-basic"
                [ attribute "color" "white"
                , boolAttr "transparent" True
                , floatAttr "opacity" 0.5
                ]
                []
        ]


meshWireframe : Mesh -> KeyCel -> Three msg
meshWireframe mesh key =
    let
        vertices =
            Encode.array Encode.vec3 <|
                addMorph key.morph key.z mesh.vertices

        faces =
            Encode.array encodeFace mesh.faces

        uvs =
            Encode.array encodeUVVec mesh.mapping
    in
    Html.node "three-mesh"
        [ floatAttr "render-order" 1 ]
        [ Html.node "geometry-buffer"
            [ property "vertices" vertices
            , property "faces" faces
            ]
            []
        , Html.node "material-mesh-basic"
            [ attribute "color" "#E53E3E"
            , boolAttr "transparent" True
            , floatAttr "opacity" 0.5
            , boolAttr "wireframe" True
            ]
            []
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
    let
        radius =
            case model.mode of
                MorphMode state _ ->
                    case state of
                        MorphMove _ ->
                            model.toolSettings.morphMove.radius

                MeshEditMode state _ _ ->
                    case state of
                        PolygonMove _ ->
                            model.toolSettings.polygonMove.radius

                        PolygonDraw _ ->
                            model.toolSettings.polygonDraw.radius

                        PolygonErase _ ->
                            model.toolSettings.polygonErase.radius
    in
    node "three-line-segments"
        [ position <|
            cursorPosition model model.cursor.position
        ]
        [ node "geometry-edges"
            []
            [ node "geometry-circle"
                [ floatAttr "radius" radius
                , intAttr "segments" <| clamp 8 64 <| round <| radius * 20 + 10
                ]
                []
            ]
        , node "material-line-basic" [ attribute "color" "white" ] []
        ]
