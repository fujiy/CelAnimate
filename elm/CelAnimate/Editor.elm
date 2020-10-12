module CelAnimate.Editor exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Array.More as Array
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import Debug
import KdTree exposing (KdTree)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type ToolState
    = PolygonDraw PolygonDrawState


initToolState : ToolState
initToolState =
    PolygonDraw initPolygonDrawState


type alias PolygonDrawState =
    { kdTree : KdTree Float ( Int, Vec3 )
    , polygons : Faces
    }


initPolygonDrawState : PolygonDrawState
initPolygonDrawState =
    { kdTree = KdTree.empty verticeToArray
    , polygons = Array.empty
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


type alias Tool =
    { center : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }


type alias ToolSettings =
    { polygonDraw : { radius : Float }
    }


initToolSettings : ToolSettings
initToolSettings =
    { polygonDraw = { radius = 0.1 }
    }


drawPolygons : ToolSettings -> Tool -> PolygonDrawState -> PolygonDrawState
drawPolygons settings tool state =
    let
        radius =
            settings.polygonDraw.radius

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
                                    if intersect then
                                        Nothing

                                    else if isFront p q new then
                                        Just ( i, j, k )

                                    else
                                        Just ( j, i, k )
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



-- drawPolygons : ToolSettings -> Tool -> PolygonDrawState -> PolygonDrawState
-- drawPolygons settings tool state =
--     let
--         radius =
--             settings.polygonDraw.radius
--         targets : Array IndexedVertice
--         targets =
--             KdTree.inRadius radius ( 0, tool.center ) state.kdTree
--         _ =
--             Debug.log "N" <| Array.length targets
--     in
--     case Array.length targets of
--         0 ->
--             let
--                 i =
--                     KdTree.size state.kdTree
--                 newPoints =
--                     Array.fromList
--                         [ ( i, tool.center ) ]
--             in
--             -- drawPolygons settings
--             -- tool
--             { state
--                 | kdTree =
--                     KdTree.insertUnbalanced newPoints state.kdTree
--             }
--         1 ->
--             let
--                 ( _, point ) =
--                     get_ 0 targets
--                 direction =
--                     if Vec3.distance tool.center point == 0 then
--                         tool.u
--                     else
--                         Vec3.direction tool.center point
--                 i =
--                     KdTree.size state.kdTree
--                 newPoints =
--                     Array.fromList
--                         [ ( i
--                           , direction
--                                 |> Vec3.scale (radius * 0.8)
--                                 |> Vec3.add point
--                           )
--                         ]
--             in
--             -- drawPolygons settings
--             --     tool
--             { state
--                 | kdTree =
--                     KdTree.insertUnbalanced newPoints state.kdTree
--             }
--         n ->
--             let
--                 normal =
--                     Vec3.cross tool.u tool.v
--                 pairs =
--                     productArray targets targets
--                         |> Array.filter
--                             (\( ( _, p ), ( _, q ) ) ->
--                                 let
--                                     dist =
--                                         Vec3.distance p q
--                                 in
--                                 0 < dist && dist < radius
--                             )
--                 triangles =
--                     Array.filterMap
--                         (\( ( i, p ), ( j, q ) ) ->
--                             let
--                                 r =
--                                     makeTriangle normal p q
--                                 near =
--                                     KdTree.inRadius (radius / 2)
--                                         ( 0, r )
--                                         state.kdTree
--                             in
--                             if Array.isEmpty near then
--                                 Just ( ( i, p ), ( j, q ), r )
--                             else
--                                 Nothing
--                         )
--                         pairs
--             in
--             if Array.isEmpty triangles then
--                 state
--             else
--                 let
--                     newI i =
--                         i + KdTree.size state.kdTree
--                     newPoints =
--                         Array.indexedMap
--                             (\i ( _, _, r ) -> ( newI i, r ))
--                             triangles
--                     polygons =
--                         Debug.log "polygons" <|
--                             Array.indexedMap
--                                 (\k ( ( i, _ ), ( j, _ ), _ ) ->
--                                     ( i, j, newI k )
--                                 )
--                                 triangles
--                     newState =
--                         { state
--                             | kdTree =
--                                 KdTree.insertUnbalanced newPoints state.kdTree
--                             , polygons = Array.append state.polygons polygons
--                         }
--                 in
--                 newState
-- drawPolygons settings tool newState


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


verticeToArray : IndexedVertice -> Array Float
verticeToArray ( i, v ) =
    let
        r =
            Vec3.toRecord v
    in
    Array.fromList [ r.x, r.y, r.z ]
