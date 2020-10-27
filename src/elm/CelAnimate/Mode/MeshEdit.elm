module CelAnimate.Mode.MeshEdit exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Math.Vector3 as Vec3
import Maybe.Extra as Maybe


type alias Result =
    { progress : MeshEditToolState
    , using : Bool
    , commit : Maybe (Image -> Mesh)
    }


clearKeyframe : Keyframe -> Keyframe
clearKeyframe keyframe =
    { keyframe | mesh = emptyMesh }


start : Keyframe -> ModeState
start keyframe =
    MeshEditMode initMeshEditTool True keyframe.mesh


finish : Mesh -> Selection -> Data -> Data
finish mesh selection =
    updateKeyframe selection (\keyframe -> { keyframe | mesh = mesh })


inProgress : MeshEditToolState -> Result
inProgress state =
    Result state True Nothing


progress : Image -> MeshEditToolState -> Mesh
progress image toolState =
    case toolState of
        PolygonMove state ->
            PolygonMove.progress image state

        PolygonDraw state ->
            PolygonDraw.progress image state

        PolygonErase state ->
            PolygonErase.progress image state


input :
    ToolSettings
    -> MeshEditToolState
    -> ToolMsg
    -> Mesh
    -> Result
input settings toolState msg mesh =
    case toolState of
        PolygonDraw state ->
            case msg.event of
                ToolStart ->
                    inProgress <|
                        PolygonDraw <|
                            PolygonDraw.start
                                settings.polygonDraw
                                msg.tool
                                mesh

                ToolMove ->
                    inProgress <|
                        PolygonDraw <|
                            PolygonDraw.step
                                settings.polygonDraw
                                msg.tool
                                state

                ToolFinish ->
                    { progress = PolygonDraw PolygonDraw.initState
                    , using = False
                    , commit = Just <| \image -> PolygonDraw.finish image state
                    }

                _ ->
                    Result toolState False Nothing

        PolygonErase state ->
            case msg.event of
                ToolStart ->
                    inProgress <|
                        PolygonErase <|
                            PolygonErase.start
                                settings.polygonErase
                                msg.tool
                                mesh

                ToolMove ->
                    inProgress <|
                        PolygonErase <|
                            PolygonErase.step
                                settings.polygonErase
                                msg.tool
                                state

                ToolFinish ->
                    { progress = PolygonErase PolygonErase.initState
                    , using = False
                    , commit = Just <| \image -> PolygonErase.finish image state
                    }

                _ ->
                    Result toolState False Nothing

        PolygonMove state ->
            case msg.event of
                ToolStart ->
                    inProgress <|
                        PolygonMove <|
                            PolygonMove.start
                                settings.polygonMove
                                msg.tool
                                mesh

                ToolMove ->
                    inProgress <|
                        PolygonMove <|
                            PolygonMove.step
                                settings.polygonMove
                                msg.tool
                                state

                ToolFinish ->
                    { progress = PolygonMove PolygonMove.initState
                    , using = False
                    , commit = Just <| \image -> PolygonMove.finish image state
                    }

                _ ->
                    Result toolState False Nothing
