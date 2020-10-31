module CelAnimate.Editor exposing (..)

import Array
import Array.Extra as Array
import Array.More as Array
import Browser.Dom exposing (getViewport)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Editor.Outliner as Outliner
import CelAnimate.Editor.Property as PropertyEditor
import CelAnimate.Editor.Timeline as Timeline
import CelAnimate.Editor.Tool as Tool
import CelAnimate.Editor.Viewport as Viewport
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import CelAnimate.Mode.Morph as Morph
import Dict
import File
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Maybe.Extra as Maybe
import Platform.Cmd
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewportSize = { width = 800, height = 600 }
      , camera = initCameraState
      , mode = Morph.initState
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
      , selection = Path -1 -1 -1
      , parameters = Dict.empty
      }
    , Cmd.none
      -- , Task.perform
      --     (\{ viewport } ->
      --         ViewportResized
      --             (round viewport.width)
      --             (round viewport.height)
      --     )
      -- getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SelectData selection ->
            ( { model | selection = selection }
            , Cmd.none
            )

        ModifyData modify ->
            ( { model | data = modify model.selection model.data }
            , Cmd.none
            )

        ChangeParameters pv ->
            ( { model
                | parameters = Dict.union pv model.parameters
              }
            , Cmd.none
            )

        SwitchMode mode ->
            ( { model | mode = mode }, Cmd.none )

        ToolSet settings ->
            ( { model | toolSettings = settings }, Cmd.none )

        ToolInput msg ->
            case model.mode of
                MeshEditMode state _ mesh ->
                    let
                        result =
                            MeshEdit.input model.toolSettings
                                state
                                msg
                                mesh

                        image =
                            Maybe.unwrap zeroImage .image <|
                                selectedCel model.selection model.data
                    in
                    ( { model
                        | mode =
                            MeshEditMode result.progress result.using <|
                                Maybe.unwrap mesh (\f -> f image) result.commit
                      }
                    , Cmd.none
                    )

                MorphMode state _ ->
                    case
                        ( selectedCel model.selection model.data
                        , selectedKeyCel model.selection model.data
                        )
                    of
                        ( Just cel, Just keycel ) ->
                            let
                                result =
                                    Morph.input model.toolSettings
                                        msg
                                        cel.mesh
                                        state
                                        keycel.morph

                                upd kc =
                                    case result.commit of
                                        Just morph ->
                                            { kc | morph = morph }

                                        Nothing ->
                                            kc
                            in
                            ( { model
                                | mode =
                                    MorphMode result.progress result.using
                                , data =
                                    updateKeyCel model.selection
                                        upd
                                        model.data
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        Pointer pe event ->
            let
                ( cursor, tmsg ) =
                    Tool.pointer pe event model
            in
            ( { model | cursor = cursor }
            , Task.perform (\_ -> ToolInput tmsg) (Task.succeed ())
            )

        ViewportResized w h ->
            let
                camera =
                    model.camera
            in
            ( { model
                | viewportSize = { width = w, height = h }
                , camera = { camera | aspect = toFloat w / toFloat h }
              }
            , Cmd.none
            )

        FileAction msg ->
            case msg of
                FileSelect ->
                    let
                        load file =
                            file
                    in
                    ( model
                    , Cmd.map (FileAction << FileSelected) <|
                        Select.file [ "image/jpeg image/png" ] load
                    )

                FileSelected file ->
                    ( model
                    , File.toUrl file
                        |> Task.perform (FileLoaded file >> FileAction)
                    )

                FileLoaded file url ->
                    let
                        updateImage image =
                            { image
                                | file = Just file
                                , src = url
                            }
                    in
                    ( { model
                        | data =
                            updateCel model.selection
                                (\keyframe ->
                                    { keyframe
                                        | image = updateImage keyframe.image
                                    }
                                )
                                model.data
                      }
                    , Cmd.none
                    )

                GotImageSize path size ->
                    let
                        setSize image =
                            { image | size = size }
                    in
                    ( { model
                        | data =
                            updateCel path
                                (\keyframe ->
                                    { keyframe | image = setSize keyframe.image }
                                )
                                model.data
                      }
                    , Cmd.none
                    )

        Batch a b ->
            ( model
            , Cmd.batch
                [ Task.perform (\_ -> a) (Task.succeed ())
                , Task.perform (\_ -> b) (Task.succeed ())
                ]
            )


view : Model -> Html Msg
view model =
    div [ class """flex flex-row w-screen h-screen text-white select-none 
                   relative text-sm bg-gray-800""" ]
        [ Tool.leftBar model.mode
        , div [ class "flex flex-col flex-1 h-screen" ]
            [ div [ class "flex flex-row h-3-4" ]
                [ div [ class "flex flex-col h-full" ]
                    [ Outliner.view model.selection model.data
                    , Tool.properties model.mode model.toolSettings
                    ]
                , div [ class "flex flex-col flex-auto" ]
                    [ Tool.topBar model.mode
                    , Viewport.view model
                    ]
                , PropertyEditor.view model.parameters
                    model.selection
                    model.data
                ]
            , Timeline.view model.parameters model.selection model.data
            ]
        ]


subs : Model -> Sub Msg
subs model =
    Sub.none
