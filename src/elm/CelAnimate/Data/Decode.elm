module CelAnimate.Data.Decode exposing (..)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import File exposing (File)
import Json.Decode exposing (..)
import List.Extra as List
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe.Extra as Maybe

-- decode : Bytes -> Maybe (Data, List (String, File))
-- decode =
--     Bytes.decode
--         <| Bytes.map2 Tuple.pair decodeData decodeImages


-- decodeImages : Bytes.Decoder (List (String, File))
-- decodeImages =
--     Bytes.unsignedInt32 LE
--         |> Bytes.andThen
--            (\size ->
--                 Bytes.loop (size, [])
--                 (\(n, rs) ->
--                      if n <= 0 then
--                          Bytes.succeed <| Bytes.Done rs
--                      else
--                          Bytes.map
--                      (\r -> Bytes.Loop (n - 1, r :: rs))
--                      decodeImage
--                 )
--            )

-- decodeImage : Bytes.Decoder (String, File)
-- decodeImage =
--     Bytes.map2 Tuple.pair
--     (Bytes.unsignedInt32 LE
--         |> Bytes.andThen Bytes.string)
--     (Bytes.unsignedInt32 LE
--         |> Bytes.andThen Bytes.string
--         |> Bytes.andThen
--          (\img ->
--               case decodeString File.decoder img of
--                   Ok file -> Bytes.succeed file
--                   Err e -> let _ = Debug.log "err" e in Bytes.fail
--                   )
--     )

-- decodeData : Bytes.Decoder Data
-- decodeData =
--     Bytes.unsignedInt32 LE
--         |> Bytes.andThen Bytes.string
--         |> Bytes.andThen
--            (\str ->
--                 case decodeString data str of
--                     Ok d -> Bytes.succeed d
--                     Err _ -> Bytes.fail
--            )

decode : Value -> Maybe (Data, List (Path, String, String))
decode =
    decodeValue
    (map2
        (\d images ->
             (d
             , List.map
             (\(path, uri) ->
                  let ps = String.split "/" path
                      init = List.init ps |> Maybe.withDefault []
                      last = List.last ps |> Maybe.withDefault ""
                  in ( parsePath d <| String.join "/" init, last, uri)
             ) images
             )
        )
        decodeData
        decodeImages
    )
    >> Result.toMaybe

decodeImages : Decoder (List (String, String))
decodeImages =
    list
    (field "type" string
        |> andThen
           (\s ->
                if s == "png" then
                    map2 Tuple.pair (field "path" string) (field "data" string)
                else
                    fail "is not png"
           )
        |> maybe
    )
    |> map Maybe.values

decodeData : Decoder Data
decodeData  =
    list
    (field "path" string
    |> andThen
         (\s ->
              if s == "data.json" then
                  field "data" data
              else
                  fail "is not data.json"
         )
    |> maybe
    )
    |> map Maybe.orList
    |> andThen (Maybe.unwrap (fail "no") succeed)

data : Decoder Data
data =
    map2 Data
        (field "name" string)
        (field "parts" <| array part)

part : Decoder Part
part =
    map4 Part
        (field "name" string)
        (field "cels" <| array cel)
        (field "parameters" <| dict parameterDesc)
        (field "keyframes" <| array keyframe)

cel : Decoder Cel
cel =
    map4 Cel
        (field "name" string)
        (field "image" image)
        (field "mesh" mesh)
        (succeed zeroInterpolate)

keyframe : Decoder Keyframe
keyframe =
    map3 Keyframe
        (field "name" string)
        (field "vector" <| dict float)
        (field "cels" <| list keycel)

keycel : Decoder KeyCel
keycel =
    map5 KeyCel
        (field "name" string)
        (field "morph" <| array vec3)
        (field "opacity" float)
        (field "show" bool)
        (field "z" float)

image : Decoder Image
image =
    map (\name -> Image name "" (0, 0) 500) string

parameter : Decoder Parameter
parameter =
    map2 Parameter
        (field "desc" parameterDesc)
        (field "value" float)

parameterDesc : Decoder ParameterDesc
parameterDesc =
    map2 ParameterDesc
        (field "name" string)
        (field "kind" parameterKind)

parameterKind : Decoder ParameterKind
parameterKind =
    oneOf
        [ map (always Open) <| text "open"
        , map3 (\_ min max -> Between {min = min, max = max})
            (field "type" <| text "between")
            (field "min" float)
            (field "max" float)
        , map3 (\_ from to -> Cyclic {from = from, to = to})
            (field "type" <| text "cyclic")
            (field "from" float)
            (field "to" float)
        , map2 (always Enum)
            (field "type" <| text "enum")
            (field "number" int)
        ]

mesh : Decoder Mesh
mesh =
    map3 Mesh
        (field "vertices" <| array vec3)
        (field "faces" <| array face)
        (field "mapping" <| array vec2)

face : Decoder Face
face =
    andThen
    (\xs ->
         case xs of
             [i, j, k] -> succeed (i, j, k)
             _ -> fail "Needs three items"
    )
    <| list int

vec2 : Decoder Vec2
vec2 =
    andThen
    (\xs ->
         case xs of
             [x, y] -> succeed <| Vec2.vec2 x y
             _ -> fail "Needs two items"
    )
    <| list float

vec3 : Decoder Vec3
vec3 =
    andThen
    (\xs ->
         case xs of
             [x, y, z] -> succeed <| Vec3.vec3 x y z
             _ -> fail "Needs three items"
    )
    <| list float

text : String -> Decoder ()
text str =
    andThen
    (\s -> if s == str then succeed () else fail "not matched")
        string
