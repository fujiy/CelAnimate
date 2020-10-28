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
    { cel : Int
    , keyframe : Int
    }


type alias Selection =
    Path


type alias Data =
    { name : String
    , cels : Array Cel
    }


type alias Cel =
    { name : String
    , keyframes : Array Keyframe
    , parameters : Dict String ParameterDesc
    }


type alias Keyframe =
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
    , cels = Array.empty
    }


zeroCel : Cel
zeroCel =
    { name = "cel"
    , keyframes = Array.fromList [ zeroKeyframe ]
    , parameters = Dict.empty
    }


zeroKeyframe : Keyframe
zeroKeyframe =
    { name = "keyframe0"
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


matchCel : Path -> Path -> Bool
matchCel a b =
    a.cel == b.cel


matchKeyframe : Path -> Path -> Bool
matchKeyframe a b =
    a.cel == b.cel && a.keyframe == b.keyframe


selectedCel : Path -> Data -> Maybe Cel
selectedCel selection data =
    Array.get selection.cel data.cels


selectedKeyframe : Path -> Data -> Maybe Keyframe
selectedKeyframe selection data =
    selectedCel selection data
        |> Maybe.andThen (.keyframes >> Array.get selection.keyframe)


updateCel : Path -> (Cel -> Cel) -> Data -> Data
updateCel selection f data =
    { data | cels = Array.update selection.cel f data.cels }


updateKeyframe : Path -> (Keyframe -> Keyframe) -> Data -> Data
updateKeyframe selection f data =
    let
        update cel =
            { cel
                | keyframes =
                    Array.update selection.keyframe f cel.keyframes
            }
    in
    updateCel selection update data


newCel : Path -> Data -> Data
newCel _ data =
    let
        cel =
            { zeroCel
                | name = "cel" ++ String.fromInt (Array.length data.cels)
            }
    in
    { data | cels = Array.push cel data.cels }


deleteCel : Path -> Data -> Data
deleteCel at data =
    { data | cels = Array.removeAt at.cel data.cels }


newKeyframe : Path -> Data -> Data
newKeyframe at data =
    let
        k =
            selectedCel at data
                |> Maybe.unwrap 0 (.keyframes >> Array.length)

        keyframe =
            { zeroKeyframe
                | name =
                    "keyframe" ++ String.fromInt k
            }
    in
    updateCel at
        (\c -> { c | keyframes = Array.push keyframe c.keyframes })
        data


deleteKeyframe : Path -> Data -> Data
deleteKeyframe at =
    updateCel at
        (\cel ->
            { cel
                | keyframes =
                    Array.removeAt at.keyframe cel.keyframes
            }
        )
