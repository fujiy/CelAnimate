module CelAnimate.Tool.MorphMove exposing (..)

import Array as Array
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import Dict exposing (Dict)
import KdTree exposing (KdTree)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set


type alias Settings =
    { radius : Float }


defaultSettings =
    { radius = 1 }


type alias State =
    { morph : Morphing
    , vertices : Vertices
    }


initState : State
initState =
    { morph = Array.empty
    , vertices = Array.empty
    }


start : Settings -> Tool -> Mesh -> Morphing -> State
start settings tool mesh morph =
    { morph =
        if Array.length mesh.vertices /= Array.length morph then
            Array.repeat (Array.length mesh.vertices) zero

        else
            morph
    , vertices = mesh.vertices
    }


step : Settings -> Tool -> State -> State
step settings tool state =
    let
        kdTree =
            KdTree.build verticeToArray <|
                Array.indexedMap Tuple.pair <|
                    addMorph state.morph 0 state.vertices

        targets =
            KdTree.inRadius settings.radius ( 0, tool.center ) kdTree

        moves =
            Set.fromList <| Array.mapToList Tuple.first targets
    in
    { state
        | morph =
            Array.indexedMap
                (\i v ->
                    if Set.member i moves then
                        Vec3.add tool.movement v

                    else
                        v
                )
                state.morph
    }


progress : State -> Morphing
progress =
    .morph


finish : State -> Morphing
finish =
    progress
