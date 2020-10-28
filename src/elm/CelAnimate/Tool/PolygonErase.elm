module CelAnimate.Tool.PolygonErase exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import KdTree exposing (KdTree)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Set exposing (Set)


type alias Settings =
    { radius : Float }


type alias State =
    { kdTree : KdTree Float ( Index, Vec3 )
    , polygons : Faces
    , deletions : Set Index
    , using : Bool
    }


initState : State
initState =
    { kdTree = KdTree.empty verticeToArray
    , polygons = Array.empty
    , deletions = Set.empty
    , using = False
    }


start : Settings -> Tool -> Mesh -> State
start settings tool mesh =
    { kdTree =
        KdTree.build verticeToArray <|
            Array.indexedMap Tuple.pair mesh.vertices
    , polygons = mesh.faces
    , deletions = Set.empty
    , using = True
    }


step : Settings -> Tool -> State -> State
step settings tool state =
    let
        targets =
            KdTree.inRadius (settings.radius * 0.5)
                ( 0, tool.center )
                state.kdTree
                |> Array.toList
                |> List.map Tuple.first
                |> Set.fromList
    in
    { state | deletions = Set.union state.deletions targets }


finish : Image -> State -> Mesh
finish =
    progress


progress : Image -> State -> Mesh
progress image state =
    let
        remaining ( i, j, k ) =
            not
                (Set.member i state.deletions
                    || Set.member j state.deletions
                    || Set.member k state.deletions
                )

        remainFaces =
            Array.filter remaining state.polygons

        remainIndices =
            Array.toList remainFaces
                |> List.concatMap (\( i, j, k ) -> [ i, j, k ])
                |> Set.fromList

        convertionList n j indices =
            case indices of
                [] ->
                    []

                i :: xs ->
                    if n < i then
                        Nothing :: convertionList (n + 1) j indices

                    else
                        Just j :: convertionList (n + 1) (j + 1) xs

        convertions =
            convertionList 0 0 (Set.toList remainIndices)
                |> Array.fromList

        convert i =
            Array.get i convertions
                |> Maybe.join
                |> Maybe.withDefault -1
    in
    uvMap image.size
        image.ppm
        { vertices =
            KdTree.toList state.kdTree
                |> List.sortBy Tuple.first
                |> List.filterMap
                    (\( i, v ) ->
                        if Set.member i remainIndices then
                            Just v

                        else
                            Nothing
                    )
                |> Array.fromList
        , faces =
            remainFaces
                |> Array.map (\( i, j, k ) -> ( convert i, convert j, convert k ))
        , mapping = Array.empty
        }
