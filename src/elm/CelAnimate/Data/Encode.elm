module CelAnimate.Data.Encode exposing (..)

import Array
import Array.Extra as Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Bytes exposing (Encoder)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import File exposing (File)
import Json.Encode as Json exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Task exposing (Task)



-- encode : Data -> Task x Bytes
-- encode d =
--     Task.map
--         (\images ->
--             [ encodeData d, images ]
--                 |> Bytes.sequence
--                 |> Bytes.encode
--         )
--     <|
--         encodeImages d
-- encodeImages : Data -> Task x Encoder
-- encodeImages d =
--     let
--         images =
--             allImages <| pathNameImages d
--     in
--     List.filterMap
--         (\image ->
--             Maybe.map
--                 (\file ->
--                     File.toBytes file
--                         |> Task.map (Tuple.pair image.src)
--                 )
--                 image.file
--         )
--         images
--         |> Task.sequence
--         |> Task.map
--             (\files ->
--                 Bytes.sequence <|
--                     Bytes.unsignedInt32 LE (List.length files)
--                         :: List.map (uncurry encodeImage) files
--             )
-- encodeImage : String -> Bytes -> Encoder
-- encodeImage src file =
--     Bytes.sequence
--         [ Bytes.unsignedInt32 LE <| Bytes.getStringWidth src
--         , Bytes.string src
--         , Bytes.unsignedInt32 LE <| Bytes.width file
--         , Bytes.bytes file
--         ]
-- encodeData : Data -> Encoder
-- encodeData d =
--     let
--         str =
--             Json.encode 0 <| data <| pathNameImages d
--     in
--     Bytes.sequence
--         [ Bytes.unsignedInt32 LE <| Bytes.getStringWidth str
--         , Bytes.string str
--         ]
-- pathNameImages : Data -> Data
-- pathNameImages d =
--     let
--         goPart p =
--             { p | cels = Array.map (goCel p.name) p.cels }
--         goCel path c =
--             { c | image = goImage (path ++ "/" ++ c.name) c.image }
--         goImage path i =
--             { i | src = path ++ "/" ++ imageName i }
--     in
--     { d | parts = Array.map goPart d.parts }


encode : Data -> Value
encode d =
    list identity <|
        object
            [ ( "path", string "data.json" )
            , ( "data", data d )
            , ( "type", string "json" )
            ]
            :: List.map
                (\( path, i ) ->
                    object
                        [ ( "path", string path )
                        , ( "data", string i.uri )
                        , ( "type", string "png" )
                        ]
                )
                (allImages d)


allImages : Data -> List ( String, Image )
allImages d =
    Array.mapToList
        (\p ->
            Array.mapToList
                (\c ->
                    ( p.name ++ "/" ++ c.name ++ "/" ++ c.image.name
                    , c.image
                    )
                )
                p.cels
        )
        d.parts
        |> List.concat


data : Data -> Value
data d =
    object
        [ ( "name", string d.name )
        , ( "parts", array part d.parts )
        ]


part : Part -> Value
part p =
    object
        [ ( "name", string p.name )
        , ( "cels", array cel p.cels )
        , ( "parameters", dict identity parameterDesc p.parameters )
        , ( "keyframes", array keyframe p.keyframes )
        ]


cel : Cel -> Value
cel c =
    object
        [ ( "name", string c.name )
        , ( "image", image c.image )
        , ( "mesh", mesh c.mesh )
        ]


image : Image -> Value
image i =
    object
        [ ( "name", string i.name )
        , ( "ppm", float i.ppm )
        ]


keyframe : Keyframe -> Value
keyframe k =
    object
        [ ( "name", string k.name )
        , ( "vector", dict identity float k.vector )
        , ( "cels", list keycel k.cels )
        ]


keycel : KeyCel -> Value
keycel k =
    object
        [ ( "name", string k.name )
        , ( "morph", array vec3 k.morph )
        , ( "opacity", float k.opacity )
        , ( "show", bool k.show )
        , ( "z", float k.z )
        ]


parameter : Parameter -> Value
parameter p =
    object
        [ ( "desc", parameterDesc p.desc )
        , ( "value", float p.value )
        ]


parameterDesc : ParameterDesc -> Value
parameterDesc d =
    object
        [ ( "name", string d.name )
        , ( "kind", parameterKind d.kind )
        ]


parameterKind : ParameterKind -> Value
parameterKind k =
    case k of
        Open ->
            string "open"

        Between o ->
            object
                [ ( "type", string "between" )
                , ( "min", float o.min )
                , ( "max", float o.max )
                ]

        Cyclic o ->
            object
                [ ( "type", string "cyclic" )
                , ( "from", float o.from )
                , ( "to", float o.to )
                ]

        Enum n ->
            object
                [ ( "type", string "enum" )
                , ( "number", int n )
                ]


mesh : Mesh -> Value
mesh m =
    object
        [ ( "vertices", array vec3 m.vertices )
        , ( "faces", array face m.faces )
        , ( "mapping", array vec2 m.mapping )
        ]


face : Face -> Value
face ( i, j, k ) =
    list int [ i, j, k ]


vec2 : Vec2 -> Value
vec2 v =
    list float [ Vec2.getX v, Vec2.getY v ]


vec3 : Vec3 -> Value
vec3 v =
    list float [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]
