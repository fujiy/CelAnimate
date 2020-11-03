module CelAnimate.Editor exposing (..)

import Array
import Array.Extra as Array
import Array.More as Array
import Browser.Dom exposing (getViewport)
import CelAnimate.Algebra exposing (..)
import CelAnimate.Data exposing (..)
import CelAnimate.Data.Decode as Decode
import CelAnimate.Data.Encode as Encode
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
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Interpolate
import Json.Decode as Decode
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

                DataLoad value ->
                    case Decode.decode value of
                        Just (data, images) ->
                            ( {model | data = data}
                            , Cmd.batch
                                <| List.map
                                    (\(path, name, uri) ->
                                         Task.perform
                                         (\_ -> FileAction
                                              <| ImageLoaded path name uri)
                                         <| Task.succeed ()
                                    )
                                    images
                            )
                        Nothing ->
                            ( model, Cmd.none)
                -- DataSelect ->
                --     ( model
                --     , Select.file [ ] <|
                --         FileAction
                --             << DataSelected
                --     )

                -- DataSelected file ->
                --     ( model
                --     , File.toBytes file
                --         |> Task.perform (FileAction << DataLoaded file)
                --     )

                -- DataLoaded file bytes ->
                --     case Decode.decode bytes of
                --         Just (d, images) ->
                --             let
                --                 data =
                --                     { d | name = File.name file }
                --             in
                --             ({ model | data = data }
                --             , Cmd.batch <|
                --                 List.map
                --                     (\(pathName, image) ->
                --                          Task.perform
                --                          (\_ ->
                --                               FileAction <|
                --                               ImageSelected
                --                               (parsePath data pathName) image
                --                          )
                --                          <| Task.succeed ()
                --                     )
                --                     images
                --             )

                --         Nothing ->
                --             (model , Cmd.none)

                -- DataSave ->
                --     ( model
                --     , Encode.encode model.data
                --         |> Task.perform (FileAction << DataWrite)
                --     )

                -- DataWrite bytes ->
                --     ( model
                --     , Download.bytes model.data.name
                --         "application/octet-stream"
                --         bytes
                --     )

                ImageSelect ->
                    ( model
                    , Select.file [ "image/jpeg image/png" ] <|
                        FileAction
                            << ImageSelected model.selection
                    )

                ImageSelected path file ->
                    ( model
                    , File.toUrl file
                    |> Task.perform
                          (FileAction << ImageLoaded path (File.name file) )
                    )

                ImageLoaded path name uri ->
                    let
                        updateImage image =
                            { image
                                | name = name
                                , uri = uri
                            }
                    in
                    ( { model
                        | data =
                            updateCel path
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
