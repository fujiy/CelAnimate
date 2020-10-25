module CelAnimate.Tool exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Html.Events.Extra.Pointer as Pointer
import Math.Vector3 as Vec3


type alias Result =
    { progress : ToolState
    , commit : Maybe Data
    }


progress : ToolState -> Result
progress state =
    Result state Nothing


input :
    ToolSettings
    -> ToolState
    -> ToolMsg
    -> DataSelection
    -> Data
    -> Result
input settings toolState msg selection data =
    case toolState of
        PolygonDraw state ->
            case msg.event of
                ToolStart ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            progress <|
                                PolygonDraw <|
                                    PolygonDraw.start
                                        settings.polygonDraw
                                        msg.tool
                                        keyframe

                        Nothing ->
                            Result toolState Nothing

                ToolMove ->
                    progress <|
                        PolygonDraw <|
                            PolygonDraw.step
                                settings.polygonDraw
                                msg.tool
                                state

                ToolFinish ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            let
                                newKeyframe _ =
                                    PolygonDraw.finish state keyframe
                            in
                            { progress = PolygonDraw PolygonDraw.initState
                            , commit =
                                Just <|
                                    updateKeyframe selection newKeyframe data
                            }

                        Nothing ->
                            Result toolState Nothing

                _ ->
                    Result toolState Nothing

        PolygonErase state ->
            case msg.event of
                ToolStart ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            progress <|
                                PolygonErase <|
                                    PolygonErase.start
                                        settings.polygonErase
                                        msg.tool
                                        keyframe

                        Nothing ->
                            Result toolState Nothing

                ToolMove ->
                    progress <|
                        PolygonErase <|
                            PolygonErase.step
                                settings.polygonErase
                                msg.tool
                                state

                ToolFinish ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            let
                                newKeyframe _ =
                                    PolygonErase.finish state keyframe
                            in
                            { progress = PolygonErase PolygonErase.initState
                            , commit =
                                Just <|
                                    updateKeyframe selection newKeyframe data
                            }

                        Nothing ->
                            Result toolState Nothing

                _ ->
                    Result toolState Nothing

        PolygonMove state ->
            case msg.event of
                ToolStart ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            progress <|
                                PolygonMove <|
                                    PolygonMove.start
                                        settings.polygonMove
                                        msg.tool
                                        keyframe

                        Nothing ->
                            Result toolState Nothing

                ToolMove ->
                    progress <|
                        PolygonMove <|
                            PolygonMove.step
                                settings.polygonMove
                                msg.tool
                                state

                ToolFinish ->
                    case selectedKeyframe selection data of
                        Just keyframe ->
                            let
                                newKeyframe _ =
                                    PolygonMove.finish state keyframe
                            in
                            { progress = PolygonMove PolygonMove.initState
                            , commit =
                                Just <|
                                    updateKeyframe selection newKeyframe data
                            }

                        Nothing ->
                            Result toolState Nothing

                _ ->
                    Result toolState Nothing


pointer : PointerEvent -> Pointer.Event -> Model -> ( Cursor, ToolMsg )
pointer pe event model =
    let
        position =
            event.pointer.offsetPos

        cursor =
            model.cursor

        velocity =
            ( (Tuple.first position - Tuple.first cursor.position)
                * 0.5
                + Tuple.first cursor.velocity
                * 0.5
            , (Tuple.second position - Tuple.second cursor.position)
                * 0.5
                + Tuple.second cursor.velocity
                * 0.5
            )

        newCursor =
            { position = position
            , velocity = velocity
            , down =
                case pe of
                    PointerDown ->
                        True

                    PointerUp ->
                        False

                    PointerCancel ->
                        False

                    _ ->
                        cursor.down
            }

        tool =
            { center = cursorPosition model newCursor.position
            , direction = cursorVelocity model newCursor.velocity
            , u = Vec3.vec3 1 0 0
            , v = Vec3.vec3 0 1 0
            }

        toolEvent =
            case pe of
                PointerDown ->
                    ToolStart

                PointerUp ->
                    ToolFinish

                PointerCancel ->
                    ToolFinish

                PointerMove ->
                    if newCursor.down then
                        ToolMove

                    else
                        ToolHover
    in
    ( newCursor, ToolMsg toolEvent tool )
