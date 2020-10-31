module CelAnimate.Data exposing (..)

import Array as Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import Dict exposing (Dict)
import File exposing (File)
import List.Extra as List
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe as Maybe
import Maybe.Extra as Maybe


type alias Path =
    { part : Int
    , cel : Int
    , keyframe : Int
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
    , keyframes : Array Keyframe
    }


type alias Cel =
    { name : String
    , image : Image
    , mesh : Mesh
    }


type alias Keyframe =
    { name : String
    , vector : ParameterVector
    , cels : List KeyCel
    }


type alias KeyCel =
    { name : String
    , morph : Morphing
    , weight : Float
    }


type alias Image =
    { file : Maybe File
    , src : String
    , size : ( Float, Float )
    , ppm : Float
    }


type alias Tool =
    { center : Vec3
    , movement : Vec3
    , direction : Vec3
    , u : Vec3
    , v : Vec3
    }


defaultParameters : Array ParameterDesc
defaultParameters =
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
    , keyframes = Array.empty
    }


zeroCel : Cel
zeroCel =
    { name = "cel0"
    , image = zeroImage
    , mesh = emptyMesh
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


selectedKeyframe : Path -> Data -> Maybe Keyframe
selectedKeyframe selection data =
    selectedPart selection data
        |> Maybe.andThen (.keyframes >> Array.get selection.keyframe)


selectedKeyCel : Path -> Data -> Maybe KeyCel
selectedKeyCel selection data =
    Maybe.andThen2
        (\keyframe cel ->
            List.find (\keycel -> keycel.name == cel.name) keyframe.cels
        )
        (selectedKeyframe selection data)
        (selectedCel selection data)


updatePart : Path -> (Part -> Part) -> Data -> Data
updatePart selection f data =
    { data | parts = Array.update selection.part f data.parts }


updateCel : Path -> (Cel -> Cel) -> Data -> Data
updateCel selection f =
    let
        update part =
            { part
                | cels =
                    Array.update selection.cel f part.cels
            }
    in
    updatePart selection update


updateKeyframe : Path -> (Keyframe -> Keyframe) -> Data -> Data
updateKeyframe selection f =
    let
        update part =
            { part
                | keyframes =
                    Array.update selection.keyframe f part.keyframes
            }
    in
    updatePart selection update


updateKeyCel : Path -> (KeyCel -> KeyCel) -> Data -> Data
updateKeyCel selection f data =
    let
        celName =
            selectedCel selection data |> Maybe.unwrap "" .name

        update keyframe =
            { keyframe
                | cels =
                    List.updateIf (\keycel -> keycel.name == celName)
                        f
                        keyframe.cels
            }
    in
    updateKeyframe selection update data


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


newKeyframe : ParameterVector -> List Cel -> Path -> Data -> Data
newKeyframe pv cels at data =
    let
        mpart =
            selectedPart at data

        k =
            Maybe.unwrap 0 (.keyframes >> Array.length) mpart

        keyframe =
            { name = "keyfame" ++ String.fromInt k
            , vector =
                Maybe.unwrap pv
                    (\part -> extractParameters part.parameters pv)
                    mpart
            , cels =
                List.map
                    (\cel ->
                        KeyCel cel.name Array.empty 1
                    )
                    cels
            }
    in
    updatePart at
        (\part ->
            { part
                | keyframes =
                    Array.push keyframe part.keyframes
            }
        )
        data


newKeyCel : Cel -> Keyframe -> Keyframe
newKeyCel cel keyframe =
    { keyframe | cels = KeyCel cel.name Array.empty 1 :: keyframe.cels }


hasKeyCel : String -> Keyframe -> Bool
hasKeyCel name keyframe =
    List.any (\keycel -> keycel.name == name) <| keyframe.cels
