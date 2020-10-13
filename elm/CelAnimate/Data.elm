module CelAnimate.Data exposing (..)

import Array exposing (Array)
import CelAnimate.Algebra exposing (..)


type alias Data =
    { path : String
    , cels : Array Cel
    }


type alias Cel =
    { name : String
    , keyframes : Array Keyframe
    }


type alias Keyframe =
    { name : String
    , image : String
    , mesh : Mesh
    }


zeroData : Data
zeroData =
    { path = ""
    , cels = Array.empty
    }
