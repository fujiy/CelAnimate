module CelAnimate.Editor.Viewport exposing (view)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
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
            MorphMode _ using ->
                body
                    model.parameters
                    (selectedKeyframe model.selection model.data)
                    model.data.parts

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
                [ node "three-texture" [ src image.src ] [] ]
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
                            meshObject True False cel.image cel.mesh <|
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
                if using then
                    [ maybe
                        (\cel ->
                            meshObject True
                                False
                                cel.image
                                (MeshEdit.progress cel.image state)
                                zeroKeyCel
                        )
                        mcel
                    ]

                else
                    [ maybe
                        (\cel ->
                            meshObject True False cel.image mesh zeroKeyCel
                        )
                        mcel
                    ]


body : ParameterVector -> Maybe Keyframe -> Array Part -> Three msg
body pv mkeyframe parts =
    node "three-group" [] <|
        Array.mapToList (partObject pv mkeyframe) parts


partObject : ParameterVector -> Maybe Keyframe -> Part -> Three msg
partObject pv mkeyframe part =
    node "three-group" [] <|
        case mkeyframe of
            Just keyframe ->
                List.map
                    (\keycel ->
                        maybe
                            (\cel ->
                                meshObject
                                    False
                                    True
                                    cel.image
                                    cel.mesh
                                    keycel
                            )
                        <|
                            celByName keycel.name part
                    )
                    keyframe.cels

            Nothing ->
                Array.mapToList
                    (\cel ->
                        meshObject False False cel.image cel.mesh <|
                            interpolate cel.interpolation pv
                    )
                    part.cels


meshObject : Bool -> Bool -> Image -> Mesh -> KeyCel -> Three msg
meshObject showMesh translucent image mesh key =
    let
        vertices =
            Encode.array encodeVec3
                <| addMorph key.morph key.z mesh.vertices

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
            , if isLoaded image then
                Html.node "material-mesh-basic"
                    [ boolAttr "transparent" True
                    , floatAttr "opacity" <|
                        if translucent then
                            0.5

                        else if key.show then
                            key.opacity
                             else 0
                    ]
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
