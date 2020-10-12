module Array.More exposing (..)

import Array exposing (..)
import Array.Extra exposing (..)


product : Array a -> Array b -> Array ( a, b )
product xs ys =
    map (\x -> Array.map (\y -> ( x, y )) ys) xs
        |> concat


concat : Array (Array a) -> Array a
concat =
    foldr append empty


all : (a -> Bool) -> Array a -> Bool
all predicate =
    foldr (\a p -> predicate a && p) True


any : (a -> Bool) -> Array a -> Bool
any predicate =
    foldr (\a p -> predicate a || p) False


maximamBy : (a -> comparable) -> Array a -> Maybe a
maximamBy f =
    let
        go a max =
            case max of
                Nothing ->
                    Just a

                Just b ->
                    if f a > f b then
                        Just a

                    else
                        Just b
    in
    foldr go Nothing


minmumBy : (a -> comparable) -> Array a -> Maybe a
minmumBy f =
    let
        go a max =
            case max of
                Nothing ->
                    Just a

                Just b ->
                    if f a < f b then
                        Just a

                    else
                        Just b
    in
    foldr go Nothing


get_ : Int -> Array a -> a
get_ i array =
    case get i array of
        Just a ->
            a

        Nothing ->
            Debug.todo "index out of range"
