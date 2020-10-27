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
import CelAnimate.Mode.MeshEdit as MeshEdit
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import Dict
import File
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Math.Vector3 as Vec3 exposing (Vec3)
import Platform.Cmd
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewportSize = { width = 800, height = 600 }
      , camera = initCameraState
      , mode = MorphMode
      , toolSettings = initToolSettings
      , cursor = initCursor
      , data = zeroData
      , selection = Path -1 -1
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

        ChangeParameter desc value ->
            ( { model
                | parameters =
                    Dict.insert desc.name value model.parameters
              }
            , Cmd.none
            )

        SwitchMode mode ->
            ( { model | mode = mode }, Cmd.none )

        ToolInput msg ->
            let
                mode =
                    case model.mode of
                        MeshEditMode state _ mesh -> 
                            let
                                result =
                                    MeshEdit.input model.toolSettings
                                        state
                                        msg
                                        mesh
                            in
                            MeshEditMode result.progress result.using <|
                                Maybe.withDefault mesh result.commit

                        MorphMode ->
                            MorphMode
            in
            ( { model | mode = mode }
            , Cmd.none
            )

        Pointer pe event ->
            let
                ( cursor, tmsg ) =
                    pointer pe event model
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
                            updateKeyframe model.selection
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
    div [ class """flex flex-row w-screen h-screen text-white select-none 
                   relative text-sm""" ]
        [ toolBar model.mode
        , div [ class "flex flex-col flex-1 h-screen" ]
            [ div [ class "flex flex-row h-3-4" ]
                [ Outliner.view model.selection model.data
                , div [ class "flex flex-col flex-auto" ]
                    [ topToolBar model.mode
                    , Viewport.view model
                    ]
                , PropertyEditor.view model.selection model.data
                ]
            , Timeline.view model.parameters model.selection model.data
            ]
        ]


toolBar : ModeState -> Html Msg
toolBar _ =
    div
        [ class """h-screen w-8 bg-gray-700 flex flex-col
                   pointer-events-auto flex-none mr-px"""
        ]
        []


topToolBar : ModeState -> Html Msg
topToolBar mode =
    case mode of
        MorphMode ->
            div [ class "flex-none" ] []

        MeshEditMode state _ mesh ->
            div [ class "flex flex-row w-full bg-gray-800 px-px pb-px" ]
                [ Html.map (\s -> SwitchMode <| MeshEditMode s False mesh) <|
                    div [ class "flex flex-row flex-auto px-4 bg-gray-700" ]
                        [ toolIcon "arrows-alt" (toolNum state == 0) <|
                            PolygonMove PolygonMove.initState
                        , toolIcon "paint-brush" (toolNum state == 1) <|
                            PolygonDraw PolygonDraw.initState
                        , toolIcon "eraser" (toolNum state == 2) <|
                            PolygonErase PolygonErase.initState
                        ]
                , div [ class """flex flex-row-reverse text-center px-4 
                             bg-gray-700""" ]
                    [ button
                        [ class "bg-gray-800 hover:bg-gray-900 px-2 m-1"
                        , onClick <|
                            Batch (ModifyData <| MeshEdit.finish mesh)
                                (SwitchMode MorphMode)
                        ]
                        [ icon_ "check", text "Finish Mesh Editing" ]
                    ]
                ]


toolNum : MeshEditToolState -> Int
toolNum state =
    case state of
        PolygonMove _ ->
            0

        PolygonDraw _ ->
            1

        PolygonErase _ ->
            2


toolIcon : String -> Bool -> MeshEditToolState -> Html MeshEditToolState
toolIcon name now state =
    button
        [ class <|
            "w-8 h-8 select-none focus:outline-none text-xl select-none "
                ++ (if now then
                        "bg-teal-700"

                    else
                        "bg-gray-700"
                   )
        , onClick state
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


pointer : PointerEvent -> Pointer.Event -> Model -> ( Cursor, ToolMsg )
pointer pe event model =
    let
        position =
            event.pointer.offsetPos

        cursor =
            model.cursor

        velocity =
            ( (Tuple.first position - Tuple.first cursor.position)
                * 0.5
                + Tuple.first cursor.velocity
                * 0.5
            , (Tuple.second position - Tuple.second cursor.position)
                * 0.5
                + Tuple.second cursor.velocity
                * 0.5
            )

        newCursor =
            { position = position
            , velocity = velocity
            , down =
                case pe of
                    PointerDown ->
                        True

                    PointerUp ->
                        False

                    PointerCancel ->
                        False

                    _ ->
                        cursor.down
            }

        tool =
            { center = cursorPosition model newCursor.position
            , direction = cursorVelocity model newCursor.velocity
            , u = Vec3.vec3 1 0 0
            , v = Vec3.vec3 0 1 0
            }

        toolEvent =
            case pe of
                PointerDown ->
                    ToolStart

                PointerUp ->
                    ToolFinish

                PointerCancel ->
                    ToolFinish

                PointerMove ->
                    if newCursor.down then
                        ToolMove

                    else
                        ToolHover
    in
    ( newCursor, ToolMsg toolEvent tool )
