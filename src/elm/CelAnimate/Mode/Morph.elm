module CelAnimate.Mode.Morph exposing (..)

import Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Tool.MorphMove as MorphMove


type alias Result =
    { progress : MorphToolState
    , using : Bool
    , commit : Maybe Morphing
    }


initState : ModeState
initState =
    MorphMode initMorphTool False


start : KeyCel -> ModeState
start keycel =
    MorphMode initMorphTool True


finish : Morphing -> Selection -> Data -> Data
finish morph selection =
    updateKeyCel selection <| \keycel -> { keycel | morph = morph }


inProgress : MorphToolState -> Result
inProgress state =
    Result state True Nothing


progress : MorphToolState -> Morphing
progress state =
    case state of
        MorphMove st ->
            st.morph


input : ToolSettings -> ToolMsg -> Mesh -> MorphToolState -> Morphing -> Result
input settings msg mesh toolState morph =
    case toolState of
        MorphMove state ->
            case msg.event of
                ToolStart ->
                    inProgress <|
                        MorphMove <|
                            MorphMove.start settings.morphMove
                                msg.tool
                                mesh
                                morph

                ToolMove ->
                    inProgress <|
                        MorphMove <|
                            MorphMove.step settings.morphMove msg.tool state

                ToolFinish ->
                    { progress = MorphMove MorphMove.initState
                    , using = False
                    , commit = Just <| MorphMove.finish state
                    }

                _ ->
                    Result toolState False Nothing
