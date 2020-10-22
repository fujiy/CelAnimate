module CelAnimate.Data exposing (..)

import Array exposing (Array)
import CelAnimate.Algebra exposing (..)
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3)


type alias Data =
    { path : String
    , cels : Array Cel
    }


type alias Cel =
    { name : String
    , keyframes : Array Keyframe
    , parameters : Dict String ParameterDesc
    }


type alias Keyframe =
    { name : String
    , image : String
    , mesh : Mesh
    , vector : ParameterVector
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
    { path = "untitled"
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
    , image = ""
    , mesh = emptyMesh
    , vector = Dict.empty
    }


type alias Tool =
    { center : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }
