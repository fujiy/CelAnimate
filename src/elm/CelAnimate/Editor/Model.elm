module CelAnimate.Editor.Model exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import File exposing (File)
import Html.Events.Extra.Pointer as Pointer
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe


type Msg
    = ViewportResized Int Int
    | Pointer PointerEvent Pointer.Event
    | ToolInput ToolMsg
    | ToolSet ToolSettings
    | SwitchMode ModeState
    | FileAction FileMsg
    | ChangeParameters ParameterVector
    | SelectData Selection
    | ModifyData (Selection -> Data -> Data)
    | Batch Msg Msg


type PointerEvent
    = PointerDown
    | PointerMove
    | PointerUp
    | PointerCancel


type alias ToolMsg =
    { event : ToolEvent
    , tool : Tool
    }


type ToolEvent
    = ToolStart
    | ToolMove
    | ToolFinish
    | ToolHover


type FileMsg
    = FileSelect
    | FileSelected File
    | FileLoaded File String
    | GotImageSize Path ( Float, Float )


type alias Model =
    { viewportSize :
        { width : Int
        , height : Int
        }
    , camera : CameraState
    , mode : ModeState
    , toolSettings : ToolSettings
    , cursor : Cursor
    , data : Data
    , selection : Selection
    , parameters : ParameterVector
    }


type MeshEditToolState
    = PolygonDraw PolygonDraw.State
    | PolygonErase PolygonErase.State
    | PolygonMove PolygonMove.State


type ModeState
    = MeshEditMode MeshEditToolState Bool Mesh
    | MorphMode


initMeshEditTool : MeshEditToolState
initMeshEditTool =
    PolygonMove PolygonMove.initState


initModeState : ModeState
initModeState =
    MorphMode


type alias CameraState =
    { position : Vec3
    , lookAt : Vec3
    , fov : Float
    , aspect : Float
    }


initCameraState : CameraState
initCameraState =
    { position = vec3 0 0 4
    , lookAt = vec3 0 0 0
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
    { polygonDraw : PolygonDraw.Settings
    , polygonErase : PolygonErase.Settings
    , polygonMove : PolygonMove.Settings
    }


initToolSettings : ToolSettings
initToolSettings =
    { polygonDraw = { radius = 0.2 }
    , polygonErase = { radius = 0.2 }
    , polygonMove = { radius = 0.2 }
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


cursorPosition : Model -> ( Float, Float ) -> Vec3
cursorPosition model ( cx, cy ) =
    let
        size =
            viewSize model.camera (Vec3.length model.camera.position)

        ( w, h ) =
            ( toFloat model.viewportSize.width
            , toFloat model.viewportSize.height
            )

        -- ( cx, cy ) =
        --     cursor.position
        x =
            (cx - w / 2) / w * size.width

        y =
            (h / 2 - cy) / h * size.height
    in
    Vec3.vec3 x y 0


cursorVelocity : Model -> ( Float, Float ) -> Vec3
cursorVelocity model ( vx, vy ) =
    let
        size =
            viewSize model.camera (Vec3.length model.camera.position)

        ( w, h ) =
            ( toFloat model.viewportSize.width
            , toFloat model.viewportSize.height
            )

        -- ( cx, cy ) =
        --     cursor.position
        -- x =
        --     vx / w * size.width
        -- y =
        --     (0 - vy) / h * size.height
    in
    Vec3.vec3 vx (0 - vy) 0
