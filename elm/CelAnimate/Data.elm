module CelAnimate.Data exposing (..)

import Array exposing (Array)
import CelAnimate.Algebra exposing (..)


type alias Data =
    { path : String
    , cels : Array Cel
    }


type alias Cel =
    { keyframes : Array Keyframe
    }


type alias Keyframe =
    { image : String
    , mesh : Mesh
    }


zeroData : Data
zeroData =
    { path = ""
    , cels = Array.empty
    }
