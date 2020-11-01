module Interpolate exposing (Spline, constant, interpolate, makeSpline)

import Array
import Array.Extra as Array
import List.Extra as List
import Matrix exposing (Matrix)
import Maybe
import Tuple


type Polynomial
    = Polynomial Float Float Float Float


type alias Spline =
    Spline_ Polynomial


type alias Spline_ a =
    { piece : Piece a
    , begin : Float
    , end : Float
    }


type SplineMD
    = Schalar Spline
    | MultiDimentional (Spline_ PolySpline)


type alias PolySpline =
    { a : SplineMD
    , b : SplineMD
    , c : SplineMD
    , d : SplineMD
    }


type Piece a
    = Leaf a
    | Node Float (Piece a) (Piece a)



-- Calculate splines -----------------------------------------------------------

constant : Float -> Spline
constant c =
    Spline_ (buildPiece [ ( 0, Polynomial 0 0 0 c ) ]) 0 0



makeSpline : List ( Float, Float ) -> Spline
makeSpline points =
    case points of
        [] ->
            constant 0

        [ ( x, y ) ] ->
            Spline_ (buildPiece [ ( x, Polynomial 0 0 0 y ) ]) x x

        [ ( x0, y0 ), ( x1, y1 ) ] ->
            Spline_ (buildPiece [ ( x0, lerp ( x0, y0 ) ( x1, y1 ) ) ]) x0 x1

        _ ->
            let
                at i =
                    Array.get i >> Maybe.withDefault 0

                at1 i =
                    at (i + 1)

                at2 i =
                    at (i + 2)

                ( xs, ys ) =
                    List.sortBy Tuple.first points
                        |> List.unzip
                        |> Tuple.mapBoth Array.fromList Array.fromList

                maxN =
                    Array.length xs - 1

                ds =
                    ys

                hs =
                    Array.map2 (-) (Array.sliceFrom 1 xs) xs

                rowC0 =
                    1 :: List.repeat maxN 0

                rowC n1 =
                    repeatAppend n1 0 <|
                        at n1 hs
                            :: 2
                            * (at n1 hs + at1 n1 hs)
                            :: at1 n1 hs
                            :: List.repeat (maxN - n1 - 2) 0

                rowCN =
                    repeatAppend maxN 0 [ 1 ]

                cfm =
                    Matrix.mat <|
                        rowC0
                            :: initializeAppend (maxN - 1) rowC [ rowCN ]

                rvn n1 =
                    3
                        * (at2 n1 ds - at1 n1 ds)
                        / at1 n1 hs
                        - 3
                        * (at1 n1 ds - at n1 ds)
                        / at n1 hs

                rv =
                    Matrix.cvec <| 0 :: initializeAppend (maxN - 1) rvn [ 0 ]

                bs =
                    Matrix.solveV cfm rv
                        |> Matrix.toFlatList
                        |> Array.fromList

                as_ =
                    Array.initialize maxN <|
                        \i -> (at1 i bs - at i bs) / (3 * at i hs)

                cs =
                    Array.initialize maxN <|
                        \i ->
                            (at1 i ds - at i ds)
                                / at i hs
                                - at i hs
                                * (at1 i bs + 2 * at i bs)
                                / 3

                polys =
                    Array.map4 Polynomial as_ bs cs ds
                        |> Array.map2 Tuple.pair xs
                        |> Array.toList
            in
            { piece = buildPiece polys
            , begin = at 0 xs
            , end = at maxN xs
            }



-- makeSplineMD : List (List Float, Float) -> SplineMD
-- makeSplineMD points


goMakeMD :
    Int
    -> List ( Float, Lattice Float )
    -> ( List (List Float), SplineMD )
goMakeMD d lattices =
    if d <= 1 then
        let
            points =
                List.filterMap
                    (\( x, lattice ) ->
                        case lattice of
                            Point a ->
                                Just ( x, a )

                            _ ->
                                Nothing
                    )
                    lattices
        in
        ( [ List.map Tuple.first points ]
        , Schalar <| makeSpline points
        )

    else
        let
            ( xss, splines ) =
                List.filterMap
                    (\( x, lattice ) ->
                        case lattice of
                            Lattice ls ->
                                let
                                    ( gs, spline ) =
                                        goMakeMD (d - 1) ls
                                in
                                Just <| ( gs, ( x, spline ) )

                            _ ->
                                Nothing
                    )
                    lattices
                    |> List.unzip

            grid : List (List Float)
            grid =
                List.transpose xss
                    |> List.map (List.foldr mergeSorteds [])
        in
        goMakeMD (d - 1) lattices


type Lattice a
    = Point a
    | Lattice (List ( Float, Lattice a ))


toLattice : Int -> List ( List Float, a ) -> Lattice a
toLattice d points =
    let
        xps : List ( Float, List ( List Float, a ) )
        xps =
            points
                |> List.filterMap
                    (\( xs, a ) ->
                        case xs of
                            [] ->
                                Nothing

                            x :: ys ->
                                Just ( x, ( ys, a ) )
                    )
                |> List.sortBy Tuple.first
                |> List.groupWhile (\p q -> Tuple.first p == Tuple.first q)
                |> List.map (Tuple.mapBoth Tuple.first (List.map Tuple.second))
    in
    if d <= 1 then
        Lattice <|
            List.filterMap
                (\( x, ysas ) ->
                    List.head ysas
                        |> Maybe.map (\( _, a ) -> ( x, Point a ))
                )
                xps

    else
        Lattice <|
            List.map
                (\( x, ysas ) -> ( x, toLattice (d - 1) ysas ))
                xps


mergeSorteds : List comparable -> List comparable -> List comparable
mergeSorteds xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            ys

        ( _, [] ) ->
            xs

        ( x :: xs_, y :: ys_ ) ->
            if x == y then
                x :: mergeSorteds xs_ ys_

            else if x < y then
                x :: mergeSorteds xs_ ys

            else
                y :: mergeSorteds xs ys_



-- Building pieces -------------------------------------------------------------


buildPiece : List ( Float, a ) -> Piece a
buildPiece sorted =
    case goBuild (List.length sorted) sorted of
        ( Just ( _, piece ), _ ) ->
            piece

        _ ->
            -- Never happen
            buildPiece []


goBuild :
    Int
    -> List ( Float, a )
    -> ( Maybe ( Float, Piece a ), List ( Float, a ) )
goBuild n xs =
    if n == 1 then
        case xs of
            [] ->
                ( Nothing, [] )

            ( x, a ) :: ys ->
                ( Just ( x, Leaf a ), ys )

    else
        let
            ( left, ys ) =
                goBuild (n // 2) xs

            ( right, zs ) =
                goBuild (n // 2 + modBy 2 n) ys
        in
        case left of
            Nothing ->
                ( Nothing, [] )

            Just ( x, pl ) ->
                case right of
                    Nothing ->
                        ( left, ys )

                    Just ( y, pr ) ->
                        ( Just ( x, Node y pl pr ), zs )



-- Interpolation ---------------------------------------------------------------


interpolate : Spline -> Float -> Float
interpolate spline x =
    goPiece spline.begin spline.end spline.piece <|
        clamp spline.begin spline.end x


goPiece : Float -> Float -> Piece Polynomial -> Float -> Float
goPiece begin end piece x =
    case piece of
        Leaf poly ->
            polynomial poly <| x - begin

        Node c left right ->
            if x < c then
                goPiece begin c left x

            else
                goPiece c end right x


interpolateMD : SplineMD -> List Float -> Float
interpolateMD smd xs =
    case xs of
        [] ->
            0

        x :: ys ->
            case smd of
                Schalar spline ->
                    interpolate spline x

                -- MultiDimentional a b c d ->
                --     polynomial
                --         (Polynomial
                --             (interpolateMD a ys)
                --             (interpolateMD b ys)
                --             (interpolateMD c ys)
                --             (interpolateMD d ys)
                --         )
                --         x
                _ ->
                    0



-- Polynominal -----------------------------------------------------------------


polynomial : Polynomial -> Float -> Float
polynomial (Polynomial a b c d) x =
    let
        x2 =
            x * x

        x3 =
            x2 * x
    in
    a * x3 + b * x2 + c * x + d


lerp : ( Float, Float ) -> ( Float, Float ) -> Polynomial
lerp ( x0, y0 ) ( x1, y1 ) =
    let
        dydx =
            (y1 - y0) / (x1 - x0)
    in
    Polynomial 0 0 dydx (y0 - dydx * x0)



-- Utilities -------------------------------------------------------------------


repeatAppend : Int -> a -> List a -> List a
repeatAppend n a xs =
    if n <= 0 then
        xs

    else
        a :: repeatAppend (n - 1) a xs


initializeAppend : Int -> (Int -> a) -> List a -> List a
initializeAppend max f =
    let
        go n xs =
            if n >= max then
                xs

            else
                f n :: go (n + 1) xs
    in
    go 0
