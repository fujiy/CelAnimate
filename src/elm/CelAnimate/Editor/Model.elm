module CelAnimate.Editor.Model exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import File exposing (File)
import Html.Events.Extra.Pointer as Pointer
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type Msg
    = ViewportResized Int Int
    | Pointer PointerEvent Pointer.Event
    | ToolInput ToolMsg
    | ToolChange ToolState
    | FileAction FileMsg
    | ChangeParameter ParameterDesc Float
    | SelectData DataSelection
    | ModifyData (Data -> Data)
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


type alias Model =
    { viewportSize :
        { width : Int
        , height : Int
        }
    , camera : CameraState
    , toolSettings : ToolSettings
    , toolState : ToolState
    , cursor : Cursor
    , data : Data
    , dataSelection : DataSelection
    , parameters : ParameterVector
    }


type alias DataSelection =
    { cel : Int
    , keyframe : Int
    }


type ToolState
    = PolygonDraw PolygonDraw.State
    | PolygonErase PolygonErase.State
    | PolygonMove PolygonMove.State


initToolState : ToolState
initToolState =
    PolygonDraw PolygonDraw.initState


type EditorMode
    = PolygonEdit
    | MorphingEdit


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
    { polygonDraw = { radius = 0.1 }
    , polygonErase = { radius = 0.1 }
    , polygonMove = { radius = 0.1 }
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


selectedCel : DataSelection -> Data -> Maybe Cel
selectedCel selection data =
    Array.get selection.cel data.cels


selectedKeyframe : DataSelection -> Data -> Maybe Keyframe
selectedKeyframe selection data =
    selectedCel selection data
        |> Maybe.andThen (.keyframes >> Array.get selection.keyframe)


updateCel : DataSelection -> (Cel -> Cel) -> Data -> Data
updateCel selection f data =
    { data | cels = Array.update selection.cel f data.cels }


updateKeyframe : DataSelection -> (Keyframe -> Keyframe) -> Data -> Data
updateKeyframe selection f data =
    let
        update cel =
            { cel
                | keyframes =
                    Array.update selection.keyframe f cel.keyframes
            }
    in
    updateCel selection update data



-- updateCurrentCel : (Cel -> Cel) -> Model -> Model
-- updateCurrentCel f model =
--     let
--         data =
--             model.data
--     in
--     { model
--         | data =
--             { data
--                 | cels =
--                     Array.update model.dataSelection.cel f data.cels
--             }
--     }
-- currentKeyframe : Model -> Maybe Keyframe
-- currentKeyframe model =
--     currentCel model
--         |> Maybe.andThen
--             (\cel -> Array.get model.dataSelection.keyframe cel.keyframes)
-- updateCurrentKeyframe : (Keyframe -> Keyframe) -> Model -> Model
-- updateCurrentKeyframe f model =
--     let
--         update cel =
--             { cel
--                 | keyframes =
--                     Array.update model.dataSelection.keyframe f cel.keyframes
--             }
--     in
--     updateCurrentCel update model


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
