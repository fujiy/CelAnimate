module CelAnimate.Data exposing (..)

import Array as Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import Dict exposing (Dict)
import File exposing (File)
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe as Maybe
import Maybe.Extra as Maybe


type alias Path =
    { part : Int
    , cel : Int
    }


type alias Selection =
    Path


type alias Data =
    { name : String
    , parts : Array Part
    }


type alias Part =
    { name : String
    , cels : Array Cel
    , parameters : Dict String ParameterDesc
    }


type alias Cel =
    { name : String
    , image : Image
    , mesh : Mesh
    , vector : ParameterVector
    }


type alias Image =
    { file : Maybe File
    , src : String
    , size : ( Float, Float )
    , ppm : Float
    }


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


type alias Tool =
    { center : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }


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


parameters : Array ParameterDesc
parameters =
    Array.fromList
        [ ParameterDesc "yaw" <| Cyclic { from = -180, to = 180 }
        , ParameterDesc "pitch" <| Between { min = -90, max = 90 }
        , ParameterDesc "roll" <| Cyclic { from = -180, to = 180 }
        ]


zeroData : Data
zeroData =
    { name = "untitled"
    , parts = Array.empty
    }


zeroPart : Part
zeroPart =
    { name = "part0"
    , cels = Array.fromList [ zeroCel ]
    , parameters = Dict.empty
    }


zeroCel : Cel
zeroCel =
    { name = "cel0"
    , image = zeroImage
    , mesh = emptyMesh
    , vector = Dict.empty
    }


zeroImage : Image
zeroImage =
    { file = Nothing
    , src = ""
    , size = ( 0, 0 )
    , ppm = 500
    }


imageName : Image -> String
imageName image =
    Maybe.unwrap "" File.name image.file


isLoaded : Image -> Bool
isLoaded image =
    Maybe.isJust image.file


matchPart : Path -> Path -> Bool
matchPart a b =
    a.part == b.part


matchCel : Path -> Path -> Bool
matchCel a b =
    a.part == b.part && a.cel == b.cel


selectedPart : Path -> Data -> Maybe Part
selectedPart selection data =
    Array.get selection.part data.parts


selectedCel : Path -> Data -> Maybe Cel
selectedCel selection data =
    selectedPart selection data
        |> Maybe.andThen (.cels >> Array.get selection.cel)


updatePart : Path -> (Part -> Part) -> Data -> Data
updatePart selection f data =
    { data | parts = Array.update selection.part f data.parts }


updateCel : Path -> (Cel -> Cel) -> Data -> Data
updateCel selection f data =
    let
        update part =
            { part
                | cels =
                    Array.update selection.cel f part.cels
            }
    in
    updatePart selection update data


newPart : Path -> Data -> Data
newPart _ data =
    let
        part =
            { zeroPart
                | name = "part" ++ String.fromInt (Array.length data.parts)
            }
    in
    { data | parts = Array.push part data.parts }


deletePart : Path -> Data -> Data
deletePart at data =
    { data | parts = Array.removeAt at.part data.parts }


newCel : Path -> Data -> Data
newCel at data =
    let
        k =
            selectedPart at data
                |> Maybe.unwrap 0 (.cels >> Array.length)

        cel =
            { zeroCel
                | name =
                    "cel" ++ String.fromInt k
            }
    in
    updatePart at
        (\p -> { p | cels = Array.push cel p.cels })
        data


deleteCel : Path -> Data -> Data
deleteCel at =
    updatePart at
        (\part ->
            { part
                | cels =
                    Array.removeAt at.cel part.cels
            }
        )
