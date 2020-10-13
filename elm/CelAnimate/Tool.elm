module CelAnimate.Tool exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Cursor =
    { position : ( Float, Float )
    , velocity : ( Float, Float )
    , down : Bool
    }


initCursor : Cursor
initCursor =
    { position = ( 0, 0 )
    , velocity = ( 0, 0 )
    , down = False
    }


type alias Tool =
    { center : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }


type alias ToolSettings =
    { polygonDraw : { radius : Float }
    }


initToolSettings : ToolSettings
initToolSettings =
    { polygonDraw = { radius = 0.1 }
    }
