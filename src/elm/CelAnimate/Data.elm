module CelAnimate.Data exposing (..)

import Array exposing (Array)
import CelAnimate.Algebra exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3)


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
    { path = "untitled"
    , cels = Array.empty
    }


zeroCel : Cel
zeroCel =
    { name = "cel"
    , keyframes = Array.fromList [ zeroKeyframe ]
    }


zeroKeyframe : Keyframe
zeroKeyframe =
    { name = "keyframe0"
    , image = ""
    , mesh = emptyMesh
    }


type alias Tool =
    { center : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }
