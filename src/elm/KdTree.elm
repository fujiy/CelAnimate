module KdTree exposing (KdTree, PointAsArrayFn, build, empty, inRadius, insertUnbalanced, size, toArray, toList)

import Array exposing (Array)
import Array.Extra as Array
import Debug
import Maybe


type alias PointAsArrayFn number p =
    p -> Array number


type TreeNode number p
    = TreeNode
        { left : TreeNode number p
        , right : TreeNode number p
        , point : p
        , axis : Int
        , median : number
        }
    | Empty Int


type KdTree number p
    = KdTree
        { pointAsList : PointAsArrayFn number p
        , root : TreeNode number p
        , size_ : Int
        }


empty : PointAsArrayFn number p -> KdTree number p
empty p2a =
    KdTree { pointAsList = p2a, root = Empty 0, size_ = 0 }


size : KdTree number p -> Int
size (KdTree kdt) =
    kdt.size_


build : PointAsArrayFn number p -> Array p -> KdTree number p
build p2a points =
    KdTree
        { pointAsList = p2a
        , root = build_ p2a points 0
        , size_ = Array.length points
        }


build_ : PointAsArrayFn number p -> Array p -> Int -> TreeNode number p
build_ p2a points axis =
    if Array.isEmpty points then
        Empty axis

    else
        let
            medianIndex =
                Array.length points // 2

            median =
                get_ medianIndex points

            points_ =
                Array.removeAt medianIndex points

            median_ =
                p2a median

            nextAxis =
                modBy (Array.length median_) (axis + 1)

            medianValue =
                get_ axis median_

            less =
                -- Debug.log "less" <|
                Array.filter (\a -> get_ axis (p2a a) < medianValue) points_

            greater =
                -- Debug.log "greater" <|
                Array.filter (\a -> get_ axis (p2a a) >= medianValue) points_
        in
        TreeNode
            { left = build_ p2a less nextAxis
            , right = build_ p2a greater nextAxis
            , point = median
            , axis = axis
            , median = medianValue
            }


insertUnbalanced : Array p -> KdTree number p -> KdTree number p
insertUnbalanced points (KdTree kdt) =
    KdTree
        { kdt
            | root = insertUnbalanced_ kdt.pointAsList points kdt.root
            , size_ = kdt.size_ + Array.length points
        }


insertUnbalanced_ :
    PointAsArrayFn number p
    -> Array p
    -> TreeNode number p
    -> TreeNode number p
insertUnbalanced_ p2a points tn =
    case tn of
        TreeNode t ->
            let
                less =
                    Array.filter
                        (\a -> get_ t.axis (p2a a) < t.median)
                        points

                greater =
                    Array.filter
                        (\a -> get_ t.axis (p2a a) >= t.median)
                        points
            in
            TreeNode
                { t
                    | left = insertUnbalanced_ p2a less t.left
                    , right = insertUnbalanced_ p2a greater t.right
                }

        Empty n ->
            build_ p2a points n


toList : KdTree number p -> List p
toList =
    toArray >> Array.toList


toArray : KdTree number p -> Array p
toArray (KdTree kdt) =
    toArray_ kdt.root


toArray_ : TreeNode number p -> Array p
toArray_ tn =
    case tn of
        Empty _ ->
            Array.empty

        TreeNode t ->
            Array.append
                (Array.push t.point (toArray_ t.left))
                (toArray_ t.right)


inRadius : number -> p -> KdTree number p -> Array p
inRadius radius query (KdTree kdt) =
    inRadius_ kdt.pointAsList radius (kdt.pointAsList query) kdt.root []
        |> Array.fromList


inRadius_ :
    PointAsArrayFn number p
    -> number
    -> Array number
    -> TreeNode number p
    -> List p
    -> List p
inRadius_ p2a radius query tn points =
    case tn of
        Empty _ ->
            points

        TreeNode t ->
            let
                onTheLeft =
                    get_ t.axis query <= t.median

                onsidePoints =
                    inRadius_ p2a
                        radius
                        query
                        (if onTheLeft then
                            t.left

                         else
                            t.right
                        )
                        points

                offsidePoints =
                    if abs (get_ t.axis query - t.median) < radius then
                        inRadius_ p2a
                            radius
                            query
                            (if onTheLeft then
                                t.right

                             else
                                t.left
                            )
                            onsidePoints

                    else
                        onsidePoints

                currentPoints =
                    if
                        squaredDistance (p2a t.point) query
                            <= radius
                            * radius
                    then
                        t.point :: offsidePoints

                    else
                        offsidePoints
            in
            currentPoints


squaredDistance : Array number -> Array number -> number
squaredDistance a b =
    Array.map2 (\x1 x2 -> (x1 - x2) ^ 2) a b
        |> Array.foldr (+) 0


get_ : Int -> Array a -> a
get_ i array =
    case Array.get i array of
        Just a ->
            a

        Nothing ->
            Debug.todo "Out of range"
