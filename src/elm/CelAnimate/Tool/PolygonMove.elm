module CelAnimate.Tool.PolygonMove exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Array.More as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import Dict exposing (Dict)
import KdTree exposing (KdTree)
import Math.Vector3 as Vec3 exposing (Vec3)


type alias Settings =
    { radius : Float }


type alias State =
    { vertices : Vertices
    , polygons : Faces
    , movings : Dict Index Vec3
    , origin : Vec3
    , using : Bool
    }


initState : State
initState =
    { vertices = Array.empty
    , polygons = Array.empty
    , movings = Dict.empty
    , origin = zero
    , using = False
    }


start : Settings -> Tool -> Mesh -> State
start settings tool mesh =
    let
        kdTree =
            KdTree.build verticeToArray <|
                Array.indexedMap Tuple.pair mesh.vertices
    in
    { vertices = mesh.vertices
    , polygons = mesh.faces
    , movings =
        KdTree.inRadius settings.radius ( 0, tool.center ) kdTree
            |> Array.mapToList (\( i, _ ) -> ( i, zero ))
            |> Dict.fromList
    , origin = tool.center
    , using = True
    }


step : Settings -> Tool -> State -> State
step settings tool state =
    { state
        | movings =
            Dict.map
                (\_ _ -> Vec3.sub tool.center state.origin)
                state.movings
    }


progress : Image -> State -> Mesh
progress image state =
    uvMap image.size
        image.ppm
        { vertices =
            state.vertices
                |> Array.indexedMap
                    (\i x ->
                        case Dict.get i state.movings of
                            Just v ->
                                Vec3.add x v

                            Nothing ->
                                x
                    )
        , faces = state.polygons
        , mapping = Array.empty
        }


finish : Image -> State -> Mesh
finish =
    progress
