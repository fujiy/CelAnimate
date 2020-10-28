module CelAnimate.Tool.PolygonDraw exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Array.More as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import KdTree exposing (KdTree)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Settings =
    { radius : Float }


type alias State =
    { kdTree : KdTree Float ( Int, Vec3 )
    , polygons : Faces
    , drawing : Bool
    }


initState : State
initState =
    { kdTree = KdTree.empty verticeToArray
    , polygons = Array.empty
    , drawing = False
    }


start : Settings -> Tool -> Mesh -> State
start settings tool mesh =
    { kdTree =
        KdTree.build verticeToArray <|
            Array.indexedMap Tuple.pair mesh.vertices
    , polygons = mesh.faces
    , drawing = True
    }


progress : Image -> State -> Mesh
progress image state =
    uvMap image.size
        image.ppm
        { vertices =
            KdTree.toArray state.kdTree
                |> Array.sortBy Tuple.first
                |> Array.map Tuple.second
        , faces = state.polygons
        , mapping = Array.empty
        }


finish : Image -> State -> Mesh
finish =
    progress


step : Settings -> Tool -> State -> State
step settings tool state =
    let
        radius =
            settings.radius

        tip =
            Vec3.normalize tool.direction
                |> Vec3.scale minSide
                |> Vec3.sub tool.center

        targets : Array IndexedVertice
        targets =
            KdTree.inRadius radius ( 0, tool.center ) state.kdTree

        normal =
            Vec3.cross tool.u tool.v

        maxSide =
            radius * 1.8

        minSide =
            radius * 0.8

        defSide =
            radius * 1.2
    in
    case Array.length targets of
        0 ->
            if isZero tool.direction then
                state

            else
                let
                    dir =
                        Vec3.normalize tool.direction
                            |> Vec3.scale (defSide / sqrt 3)

                    rotates =
                        [ ( 0, Mat4.makeRotate (pi / 2) normal )
                        , ( 1, Mat4.makeRotate (pi * 7 / 6) normal )
                        , ( 2, Mat4.makeRotate (pi * 11 / 6) normal )
                        ]

                    newI i =
                        KdTree.size state.kdTree + i

                    rotateBy ( i, r ) =
                        ( newI i
                        , Mat4.transform r dir
                            |> Vec3.add tool.center
                        )

                    newPoints =
                        List.map rotateBy rotates
                            |> Array.fromList

                    polygon =
                        ( newI 0, newI 1, newI 2 )
                in
                { state
                    | kdTree =
                        KdTree.insertUnbalanced newPoints state.kdTree
                    , polygons = Array.push polygon state.polygons
                }

        _ ->
            let
                connects =
                    KdTree.inRadius radius ( 0, tip ) state.kdTree

                dir =
                    Vec3.normalize tool.direction
                        |> Vec3.scale defSide

                pairs points =
                    Array.filter
                        (\( ( _, p ), ( _, q ) ) ->
                            let
                                dist =
                                    Vec3.distance p q
                            in
                            0 < dist && dist < maxSide
                        )
                    <|
                        Array.product points points

                canditates =
                    Array.filterMap
                        (\( _, r ) ->
                            let
                                new =
                                    Vec3.add dir r

                                overlaps =
                                    KdTree.inRadius minSide
                                        ( 0, new )
                                        state.kdTree

                                nears =
                                    KdTree.inRadius maxSide
                                        ( 0, new )
                                        state.kdTree
                                        |> pairs

                                intersect =
                                    Array.any
                                        (\( ( _, p ), ( _, q ) ) ->
                                            intersects r new p q
                                        )
                                        nears
                            in
                            if Array.isEmpty overlaps && not intersect then
                                Just new

                            else
                                Nothing
                        )
                        connects

                newPoint =
                    Array.maximamBy
                        (Vec3.distanceSquared
                            (Vec3.add tool.center dir)
                        )
                        canditates
            in
            case newPoint of
                Nothing ->
                    state

                Just new ->
                    let
                        k =
                            KdTree.size state.kdTree

                        neighbors =
                            KdTree.inRadius maxSide
                                ( 0, new )
                                state.kdTree

                        isFront p q r =
                            let
                                norm =
                                    Vec3.cross
                                        (Vec3.sub q p)
                                        (Vec3.sub r p)
                            in
                            Vec3.dot norm normal > 0

                        nears =
                            pairs neighbors

                        polygons =
                            Array.filterMap
                                (\( ( i, p ), ( j, q ) ) ->
                                    let
                                        intersect =
                                            Array.any
                                                (\( ( _, a ), ( _, b ) ) ->
                                                    intersects a b new p
                                                        || intersects a b new q
                                                )
                                                nears
                                    in
                                    if intersect || not (isFront p q new) then
                                        Nothing

                                    else
                                        Just ( i, j, k )
                                )
                                nears

                        newPoints =
                            Array.fromList [ ( k, new ) ]
                    in
                    { state
                        | kdTree =
                            KdTree.insertUnbalanced newPoints state.kdTree
                        , polygons = Array.append state.polygons polygons
                    }
