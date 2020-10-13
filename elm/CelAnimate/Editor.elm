module CelAnimate.Editor exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Array.More as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Tool.PolygonDraw exposing (..)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Json.Encode as Json
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Three msg =
    Html msg


type ToolState
    = PolygonDraw PolygonDrawState


initToolState : ToolState
initToolState =
    PolygonDraw initPolygonDrawState


type EditorMode
    = PolygonEdit
    | MorphingEdit


type alias CameraState =
    { position : Vec3
    , fov : Float
    , aspect : Float
    }


initCameraState : CameraState
initCameraState =
    { position = vec3 0 0 4
    , fov = 50
    , aspect = 1
    }


polygonMesh : Mesh -> Three msg
polygonMesh mesh =
    let
        vertices =
            Json.array encodeVec3 mesh.vertices

        faces =
            Json.array encodeFace mesh.faces
    in
    Html.node "three-group"
        []
        [ Html.node "three-mesh"
            []
            [ Html.node "geometry-buffer"
                [ Attr.property "vertices" vertices
                , Attr.property "faces" faces
                ]
                []
            , Html.node "material-mesh-basic"
                [ Attr.attribute "color" "blue"
                , boolAttr "wireframe" True
                , floatAttr "linewidth" 2.0
                ]
                []
            ]
        , Html.node "three-mesh"
            []
            [ Html.node "geometry-buffer"
                [ Attr.property "vertices" vertices
                , Attr.property "faces" faces
                ]
                []
            , Html.node "material-mesh-basic"
                [ Attr.attribute "color" "cyan"
                , boolAttr "wireframe" False
                , boolAttr "transparent" True
                , floatAttr "opacity" 0.5
                ]
                []
            ]
        ]


viewSize : CameraState -> Float -> { width : Float, height : Float }
viewSize camera distance =
    let
        vfov =
            degrees camera.fov

        height =
            2 * tan (vfov / 2) * distance

        width =
            height * camera.aspect
    in
    { width = width, height = height }


intAttr : String -> Int -> Attribute msg
intAttr name n =
    Attr.attribute name (String.fromInt n)


floatAttr : String -> Float -> Attribute msg
floatAttr name x =
    Attr.attribute name (String.fromFloat x)


boolAttr : String -> Bool -> Attribute msg
boolAttr name b =
    Attr.attribute name
        (if b then
            "true"

         else
            ""
        )
