module CelAnimate.Algebra exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Array.More as Array
import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Encode as Encode
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe


type alias Index =
    Int


type alias UVVec =
    Vec2


type alias Vertices =
    Array Vec3


type alias IndexedVertice =
    ( Index, Vec3 )


type alias Face =
    ( Index, Index, Index )


type alias Faces =
    Array Face


type alias Morphing =
    Array Vec3


type alias Mesh =
    { vertices : Vertices
    , faces : Faces
    , mapping : Array UVVec
    }



-- Vectors ---------------------------------------------------------------------


makeTriangle : Vec3 -> Vec3 -> Vec3 -> Vec3
makeTriangle normal a b =
    let
        ab =
            Vec3.sub b a

        h =
            Vec3.cross normal ab

        ac =
            Vec3.add (Vec3.scale 0.5 ab) (Vec3.scale sqrt3p2 h)
    in
    Vec3.add a ac


sqrt3p2 =
    sqrt 3 / 2


zero : Vec3
zero =
    Vec3.vec3 0 0 0


theta : Vec3 -> Vec3 -> Float
theta u v =
    acos (Vec3.dot u v / (Vec3.length u * Vec3.length v))


isZero : Vec3 -> Bool
isZero v =
    Vec3.getX v == 0 && Vec3.getY v == 0 && Vec3.getZ v == 0


intersects : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Bool
intersects p q a b =
    let
        ( ab, ap ) =
            ( Vec3.sub b a, Vec3.sub p a )

        ( pa, pb, pq ) =
            ( Vec3.sub a p, Vec3.sub b p, Vec3.sub q p )

        ( apb, apq, bpq ) =
            ( theta pa pb, theta pa pq, theta pb pq )

        betweens =
            apq < apb && bpq < apb

        h =
            Vec3.add a <|
                Vec3.scale
                    (Vec3.dot ap ab
                        / Vec3.lengthSquared ab
                    )
                    ab

        d2 =
            Vec3.distanceSquared p h
    in
    betweens && d2 < Vec3.lengthSquared pq


encodeVec3 : Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]



-- Meshes ----------------------------------------------------------------------


emptyMesh : Mesh
emptyMesh =
    Mesh Array.empty Array.empty Array.empty


isEmptyMesh : Mesh -> Bool
isEmptyMesh mesh =
    Array.isEmpty mesh.vertices && Array.isEmpty mesh.faces


encodeFace : Face -> Encode.Value
encodeFace ( i, j, k ) =
    Encode.list Encode.int [ i, j, k ]


encodeUVVec : UVVec -> Encode.Value
encodeUVVec v =
    Encode.list Encode.float [ Vec2.getX v, Vec2.getY v ]


verticeToArray : IndexedVertice -> Array Float
verticeToArray ( i, v ) =
    let
        r =
            Vec3.toRecord v
    in
    Array.fromList [ r.x, r.y, r.z ]


uvMap : ( Float, Float ) -> Float -> Mesh -> Mesh
uvMap ( width, height ) ppm mesh =
    let
        u x =
            x * ppm / width + 0.5

        v y =
            y * ppm / height + 0.5

        map p =
            Vec2.vec2 (u <| Vec3.getX p) (v <| Vec3.getY p)
    in
    { mesh | mapping = Array.map map mesh.vertices }


addMorph : Morphing -> Float -> Vertices -> Vertices
addMorph morph z vertices =
    Array.map2 Vec3.add (Array.map (Vec3.add <| vec3 0 0 z) vertices) <|
        Array.append morph <|
            Array.repeat (Array.length vertices - Array.length morph) zero



-- Parameters ------------------------------------------------------------------


type alias Parameter =
    { desc : ParameterDesc
    , value : Float
    }


type alias ParameterVector =
    Dict String Float


type alias ParameterDesc =
    { name : String
    , kind : ParameterKind
    }


type ParameterKind
    = Open
    | Between { min : Float, max : Float }
    | Cyclic { from : Float, to : Float }
    | Enum Int


minimumValue : ParameterDesc -> Float
minimumValue desc =
    case desc.kind of
        Open ->
            -1

        Between o ->
            o.min

        Cyclic o ->
            o.from

        Enum _ ->
            1


maximumValue : ParameterDesc -> Float
maximumValue desc =
    case desc.kind of
        Open ->
            1

        Between o ->
            o.max

        Cyclic o ->
            o.to

        Enum n ->
            toFloat n


defaultValue : ParameterDesc -> Float
defaultValue desc =
    case desc.kind of
        Open ->
            0

        Between o ->
            (o.min + o.max) / 2

        Cyclic o ->
            (o.from + o.to) / 2

        Enum _ ->
            0


parameterDifference : ParameterDesc -> Float -> Float -> Float
parameterDifference desc b x =
    if x < b then
        (x - b) / (b - minimumValue desc)

    else
        (x - b) / (maximumValue desc - b)


extractParameters : Dict String ParameterDesc -> ParameterVector -> ParameterVector
extractParameters names pv =
    Dict.map
        (\name desc ->
            Dict.get name pv
                |> Maybe.withDefault (defaultValue desc)
        )
        names


getValue : ParameterDesc -> ParameterVector -> Float
getValue desc vector =
    Dict.get desc.name vector
        |> Maybe.withDefault (defaultValue desc)


parameterProjection :
    Dict String ParameterDesc
    -> ParameterVector
    -> ParameterVector
    -> ParameterDesc
    -> Float
parameterProjection names origin vector desc =
    let
        otherValues =
            Dict.values names
                |> List.filter (\d -> d.name /= desc.name)
                |> List.map
                    (\d ->
                        parameterDifference d
                            (getValue d origin)
                            (getValue d vector)
                    )

        sign =
            Maybe.unwrap 0 signum <| List.head otherValues

        distance =
            List.map (\x -> x * x) otherValues |> List.sum |> sqrt
    in
    sign * distance


rotationEuler : ParameterVector -> Vec3
rotationEuler pv =
    let
        pitch =
            Dict.get "pitch" pv |> Maybe.unwrap 0 degrees

        yaw =
            Dict.get "yaw" pv |> Maybe.unwrap 0 degrees

        roll =
            Dict.get "roll" pv |> Maybe.unwrap 0 degrees
    in
    vec3 pitch yaw roll



-- Utility Functions -----------------------------------------------------------


signum : Float -> Float
signum x =
    if x < 0 then
        -1

    else if x == 0 then
        0

    else
        1


fromJust : Maybe a -> a
fromJust m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo "Nothing"

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b
