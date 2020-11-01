module CelAnimate.Data exposing (..)

import Array as Array exposing (Array)
import Array.Extra as Array
import CelAnimate.Algebra exposing (..)
import Dict exposing (Dict)
import File exposing (File)
import Interpolate
import List.Extra as List
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
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
    , interpolation : Interpolation
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
    , z : Float
    }


type alias Interpolation =
    { morph : Array ( Interpolate.Spline, Interpolate.Spline )
    , weight : Interpolate.Spline
    , z : Interpolate.Spline
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
    , interpolation = zeroInterpolate
    }


zeroKeyCel : KeyCel
zeroKeyCel =
    { name = ""
    , morph = Array.empty
    , weight = 1
    , z = 0
    }


zeroInterpolate : Interpolation
zeroInterpolate =
    { morph = Array.empty
    , weight = Interpolate.emptySpline
    , z = Interpolate.emptySpline
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


celByName : String -> Part -> Maybe Cel
celByName name part =
    Array.get 0 <|
        Array.filter
            (\cel -> cel.name == name)
            part.cels


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
                        let
                            keycel =
                                interpolate cel.interpolation pv
                        in
                        { keycel | name = cel.name }
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


newKeyCel : ParameterVector -> Cel -> Keyframe -> Keyframe
newKeyCel pv cel keyframe =
    { keyframe
        | cels =
            let
                keycel =
                    interpolate cel.interpolation pv
            in
            { keycel | name = cel.name } :: keyframe.cels
    }


hasKeyCel : String -> Keyframe -> Bool
hasKeyCel name keyframe =
    List.any (\keycel -> keycel.name == name) <| keyframe.cels


calcInterpolationOfSelectedPart : Selection -> Data -> Data
calcInterpolationOfSelectedPart selection data =
    updateCel selection
        (\cel ->
            case selectedPart selection data of
                Just part ->
                    calcInterpolation part.parameters part.keyframes cel

                Nothing ->
                    cel
        )
        data


calcInterpolation : Dict String ParameterDesc -> Array Keyframe -> Cel -> Cel
calcInterpolation names keyframes cel =
    let
        chooseParam pv =
            extractParameters names pv
                |> Dict.values
                |> List.head
                |> Maybe.withDefault 0

        keys : List ( Float, KeyCel )
        keys =
            Array.toList keyframes
                |> List.filterMap
                    (\keyframe ->
                        List.find (\keycel -> keycel.name == cel.name)
                            keyframe.cels
                            |> Maybe.map
                                (Tuple.pair (chooseParam keyframe.vector))
                    )

        weight =
            keys
                |> List.map
                    (\( t, keycel ) -> ( t, keycel.weight ))
                |> Interpolate.makeSpline

        z =
            keys
                |> List.map
                    (\( t, keycel ) -> ( t, keycel.z ))
                |> Interpolate.makeSpline

        morph =
            keys
                |> List.map
                    (\( t, keycel ) ->
                        Array.resizerRepeat
                            (Array.length cel.mesh.vertices)
                            zero
                            keycel.morph
                            |> Array.mapToList
                                (\v ->
                                    ( ( t, Vec3.getX v ), ( t, Vec3.getY v ) )
                                )
                    )
                |> List.transpose
                |> List.map
                    (\vs ->
                        let
                            ( xs, ys ) =
                                List.unzip vs
                        in
                        ( Interpolate.makeSpline xs
                        , Interpolate.makeSpline ys
                        )
                    )
                |> Array.fromList
    in
    { cel | interpolation = Interpolation morph weight z }


interpolate : Interpolation -> ParameterVector -> KeyCel
interpolate ip pv =
    let
        t =
            Dict.values pv |> List.head |> Maybe.withDefault 0
    in
    { name = ""
    , morph =
        Array.map
            (\( xsp, ysp ) ->
                vec3 (Interpolate.interpolate xsp t)
                    (Interpolate.interpolate ysp t)
                    0
            )
            ip.morph
    , weight = Interpolate.interpolate ip.weight t
    , z = Interpolate.interpolate ip.z t
    }
