module CelAnimate.Editor.Model exposing (..)

import Array
import CelAnimate.Data exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import Math.Vector3 exposing (Vec3, vec3)


type Msg
    = WindowResized Int Int
    | CanvasPointer CanvasPointerMsg
    | DataTree DataTreeMsg
    | ToolInput ToolMsg Tool


type CanvasPointerMsg
    = PointerDown
    | PointerMove ( Float, Float )
    | PointerUp


type DataTreeMsg
    = SelectCel Int
    | NewCel Int


type ToolMsg
    = ToolStart
    | ToolMove
    | ToolFinish
    | ToolHover


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
    , dataSelection : DataSelection
    }


type alias DataSelection =
    { cel : Int
    , keyframe : Int
    }


type ToolState
    = PolygonDraw PolygonDraw.State


initToolState : ToolState
initToolState =
    PolygonDraw PolygonDraw.initState


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


type alias Cursor =
    { position : ( Float, Float )
    , velocity : ( Float, Float )
    , down : Bool
    }


initCursor : Cursor
initCursor =
    { position = ( 0, 0 )
    , velocity = ( 0, 0 )
    , down = False
    }


type alias ToolSettings =
    { polygonDraw : PolygonDraw.Settings }


initToolSettings : ToolSettings
initToolSettings =
    { polygonDraw = { radius = 0.1 }
    }


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


currentKeyframe : Model -> Maybe Keyframe
currentKeyframe model =
    model.data.cels
        |> Array.get model.dataSelection.cel
        |> Maybe.andThen
            (\cel -> Array.get model.dataSelection.keyframe cel.keyframes)
