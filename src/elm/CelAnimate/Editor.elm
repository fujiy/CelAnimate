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
import CelAnimate.Editor.Viewport as Viewport
import CelAnimate.Html exposing (..)
import CelAnimate.Tool as Tool
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Dict
import File
import File.Select as Select
import Html exposing (Html, button, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Math.Vector3 as Vec3 exposing (Vec3)
import Platform.Cmd
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewportSize = { width = 800, height = 600 }
      , camera = initCameraState
      , toolState = initToolState
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
      , dataSelection = DataSelection 0 0
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
            ( { model | dataSelection = selection }, Cmd.none )

        ModifyData modify ->
            ( { model | data = modify model.data }, Cmd.none )

        ChangeParameter desc value ->
            ( { model
                | parameters =
                    Dict.insert desc.name value model.parameters
              }
            , Cmd.none
            )

        ToolChange state ->
            ( { model | toolState = state }, Cmd.none )

        ToolInput msg ->
            let
                result =
                    Tool.input model.toolSettings
                        model.toolState
                        msg
                        model.dataSelection
                        model.data
            in
            ( { model
                | toolState = result.progress
                , data = Maybe.withDefault model.data result.commit
              }
            , Cmd.none
            )

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
                    ( { model
                        | data =
                            updateKeyframe model.dataSelection
                                (\keyframe -> { keyframe | image = Just ( file, url ) })
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
    div [ class "flex flex-row w-screen text-white select-none" ]
        [ toolBar model
        , div [ class "flex flex-col flex-grow flex-shrink" ]
            [ div [ class "flex flex-row flex-grow flex-shrink" ]
                [ Outliner.view model.data model.dataSelection
                , Viewport.view model
                , PropertyEditor.view model.dataSelection model.data
                ]
            , Timeline.view model.parameters model.dataSelection model.data
            ]
        ]


toolBar : Model -> Html Msg
toolBar model =
    let
        tool =
            case model.toolState of
                PolygonMove _ ->
                    0

                PolygonDraw _ ->
                    1

                PolygonErase _ ->
                    2
    in
    div
        [ class <|
            "h-screen w-8 bg-gray-700 flex flex-col text-xl select-none "
                ++ "pointer-events-auto flex-grow-0 flex-shrink-0"
        ]
        [ toolIcon "arrows-alt" (tool == 0) <|
            PolygonMove PolygonMove.initState
        , toolIcon "paint-brush" (tool == 1) <|
            PolygonDraw PolygonDraw.initState
        , toolIcon "eraser" (tool == 2) <|
            PolygonErase PolygonErase.initState
        ]


toolIcon : String -> Bool -> ToolState -> Html Msg
toolIcon name now state =
    button
        [ class <|
            "p-1 select-none focus:outline-none "
                ++ (if now then
                        "bg-teal-700"

                    else
                        ""
                   )
        , onClick <| ToolChange state
        ]
        [ icon name ]


cursorView : Model -> Html Msg
cursorView model =
    let
        ( x, y ) =
            model.cursor.position

        -- diameter = model.toolSettings.polygonDraw.radius * 2
    in
    div
        [ class "cursor-polygon-draw fixed"
        , style "top" (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        , style "width" "20px"
        , style "height" "20px"
        ]
        []


subs : Model -> Sub Msg
subs model =
    Sub.none
