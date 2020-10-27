module CelAnimate.Algebra exposing (..)

import Array exposing (Array)
import Array.More as Array
import Json.Encode as Encode
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)


type alias Index =
    Int

type alias UVVec = Vec2

type alias Vertices =
    Array Vec3


type alias IndexedVertice =
    ( Index, Vec3 )


type alias Face =
    ( Index, Index, Index )


type alias Faces =
    Array Face

type alias Mesh =
    { vertices : Vertices
    , faces : Faces
    , mapping : Array UVVec
    }


emptyMesh : Mesh
emptyMesh =
    Mesh Array.empty Array.empty Array.empty


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

encodeFace : Face -> Encode.Value
encodeFace ( i, j, k ) =
    Encode.list Encode.int [ i, j, k ]

encodeUVVec : UVVec -> Encode.Value
encodeUVVec v =
    Encode.list Encode.float [ Vec2.getX v, Vec2.getY v]


verticeToArray : IndexedVertice -> Array Float
verticeToArray ( i, v ) =
    let
        r =
            Vec3.toRecord v
    in
    Array.fromList [ r.x, r.y, r.z ]

uvMap : (Float, Float) -> Float -> Mesh -> Mesh
uvMap (width, height) ppm mesh =
    let u x = x * ppm / width + 0.5
        v y = y * ppm / height + 0.5
        map p =  Vec2.vec2 (u <| Vec3.getX p) (v <| Vec3.getY p)
    in
    {mesh | mapping = Array.map map mesh.vertices }


fromJust : Maybe a -> a
fromJust m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo "Nothing"
