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
import Parser as Parser exposing ((|.), (|=), Parser)


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
    , opacity : Float
    , show : Bool
    , z : Float
    }


type alias Interpolation =
    { morph : Array ( Interpolate.Spline, Interpolate.Spline )
    , opacity : Interpolate.Spline
    , show : Interpolate.Spline
    , z : Interpolate.Spline
    }


type alias Image =
    { name : String
    , uri : String
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


nullPath : Path
nullPath =
    { part = -1
    , cel = -1
    , keyframe = -1
    }


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
    , opacity = 1
    , show = True
    , z = 0
    }


zeroInterpolate : Interpolation
zeroInterpolate =
    { morph = Array.empty
    , opacity = Interpolate.constant 1
    , show = Interpolate.constant 1
    , z = Interpolate.constant 0
    }


zeroImage : Image
zeroImage =
    { name = ""
    , uri = ""
    , size = ( 0, 0 )
    , ppm = 300
    }


isLoaded : Image -> Bool
isLoaded image =
    image.uri /= ""


matchPart : Path -> Path -> Bool
matchPart a b =
    a.part == b.part


matchCel : Path -> Path -> Bool
matchCel a b =
    a.part == b.part && a.cel == b.cel



-- Get -------------------------------------------------------------------------


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


hasKeyCel : String -> Keyframe -> Bool
hasKeyCel name keyframe =
    List.any (\keycel -> keycel.name == name) <| keyframe.cels


keyCelPath : Path -> Data -> KeyCel -> Path
keyCelPath path data keycel =
    selectedPart path data
        |> Maybe.andThen
            (\part ->
                Array.toList part.cels
                    |> List.findIndex
                        (\cel -> cel.name == keycel.name)
                    |> Maybe.map
                        (\i -> { path | cel = i })
            )
        |> Maybe.withDefault path


keyCelMatches : Cel -> Keyframe -> Maybe KeyCel
keyCelMatches cel keyframe =
    List.find (\keycel -> keycel.name == cel.name) keyframe.cels



-- Map / For -------------------------------------------------------------------


forParts : Data -> (Path -> Part -> a) -> List a
forParts data f =
    Array.indexedMapToList
        (\i -> f { nullPath | part = i })
        data.parts


forCels : Path -> Part -> (Path -> Cel -> a) -> List a
forCels path part f =
    Array.indexedMapToList
        (\i -> f { path | cel = i })
        part.cels


forKeyframes : Path -> Part -> (Path -> Keyframe -> a) -> List a
forKeyframes path part f =
    Array.indexedMapToList
        (\i -> f { path | keyframe = i })
        part.keyframes



-- Update ----------------------------------------------------------------------


calcIp : Path -> Data -> Data
calcIp path =
    updatePart path calcInterpolationOfPart


updatePart : Path -> (Part -> Part) -> Data -> Data
updatePart selection f data =
    { data | parts = Array.update selection.part f data.parts }


updatePartAndCalc : Path -> (Part -> Part) -> Data -> Data
updatePartAndCalc selection f =
    updatePart selection <| f >> calcInterpolationOfPart


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


updateCelAndCalc : Path -> (Cel -> Cel) -> Data -> Data
updateCelAndCalc selection f =
    updateCel selection f >> calcIp selection


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


updateKeyframeAndCalc : Path -> (Keyframe -> Keyframe) -> Data -> Data
updateKeyframeAndCalc selection f =
    updateKeyframe selection f >> calcIp selection


updateKeyframeAndCalcWhen : Bool -> Path -> (Keyframe -> Keyframe) -> Data -> Data
updateKeyframeAndCalcWhen calc =
    if calc then
        updateKeyframeAndCalc

    else
        updateKeyframe


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


updateKeyCelAndCalc : Path -> (KeyCel -> KeyCel) -> Data -> Data
updateKeyCelAndCalc selection f =
    updateKeyCel selection f >> calcIp selection


updateKeyCelAndCalcWhen : Bool -> Path -> (KeyCel -> KeyCel) -> Data -> Data
updateKeyCelAndCalcWhen calc =
    if calc then
        updateKeyCelAndCalc

    else
        updateKeyCel


setPPM : Float -> Cel -> Cel
setPPM ppm cel =
    let
        image =
            cel.image
    in
    { cel
        | image = { image | ppm = ppm }
        , mesh = uvMap image.size ppm cel.mesh
    }



-- New -------------------------------------------------------------------------


newPart : Path -> Data -> Data
newPart _ data =
    let
        part =
            { zeroPart
                | name = "part" ++ String.fromInt (Array.length data.parts)
            }
    in
    { data | parts = Array.push part data.parts }


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



-- Delete ----------------------------------------------------------------------


deletePart : Path -> Data -> Data
deletePart at data =
    { data | parts = Array.removeAt at.part data.parts }


deleteCel : Path -> Data -> Data
deleteCel at =
    updatePart at
        (\part ->
            { part
                | cels =
                    Array.removeAt at.cel part.cels
            }
        )


deleteKeyframe : Path -> Data -> Data
deleteKeyframe at =
    updatePart at
        (\part ->
            { part
                | keyframes =
                    Array.removeAt at.keyframe part.keyframes
            }
        )



-- Interpolation ---------------------------------------------------------------


calcInterpolationOfPart : Part -> Part
calcInterpolationOfPart part =
    { part
        | cels =
            Array.map
                (calcInterpolation part.parameters part.keyframes)
                part.cels
    }


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

        opacity =
            keys
                |> List.map
                    (\( t, keycel ) -> ( t, keycel.opacity ))
                |> Interpolate.makeSpline

        show =
            keys
                |> List.map
                    (\( t, keycel ) ->
                        ( t
                        , if keycel.show then
                            1

                          else
                            0
                        )
                    )
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
    { cel | interpolation = Interpolation morph opacity show z }


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
    , opacity = Interpolate.interpolate ip.opacity t
    , show =
        if Interpolate.interpolate ip.show t < 0.8 then
            False

        else
            True
    , z = Interpolate.interpolate ip.z t
    }



-- Path ------------------------------------------------------------------------


parsePath : Data -> String -> Path
parsePath d =
    Parser.run (partPathParser d)
        >> Result.mapError (Debug.log "parse error")
        >> Result.withDefault nullPath


partPathParser : Data -> Parser Path
partPathParser data =
    Parser.oneOf <|
        Array.indexedMapToList
            (\i part ->
                Parser.succeed (\path -> { path | part = i })
                    |. Parser.backtrackable (Parser.keyword part.name)
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.symbol "/"
                            |= Parser.oneOf
                                [ celPathParser part
                                , keyframePathParser part
                                , Parser.succeed nullPath |. Parser.end
                                ]
                        , Parser.succeed nullPath |. Parser.end
                        ]
            )
            data.parts


celPathParser : Part -> Parser Path
celPathParser part =
    Parser.oneOf <|
        Array.indexedMapToList
            (\i cel ->
                Parser.succeed (\path -> { path | cel = i })
                    |= endPathParser cel.name
            )
            part.cels


keyframePathParser : Part -> Parser Path
keyframePathParser part =
    Parser.oneOf <|
        Array.indexedMapToList
            (\i keyframe ->
                Parser.succeed (\path -> { path | keyframe = i })
                    |= endPathParser keyframe.name
            )
            part.keyframes


endPathParser : String -> Parser Path
endPathParser name =
    Parser.succeed nullPath
        |. Parser.backtrackable (Parser.keyword name)
        |. nullPathParser


nullPathParser : Parser Path
nullPathParser =
    Parser.succeed nullPath
        |. Parser.oneOf [ Parser.end, Parser.symbol "/" |. Parser.end ]
