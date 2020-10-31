module CelAnimate.Editor.Tool exposing (..)

import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import CelAnimate.Mode.MeshEdit as MeshEdit
import CelAnimate.Mode.Morph as Morph
import CelAnimate.Tool.MorphMove as MorphMove
import CelAnimate.Tool.PolygonDraw as PolygonDraw
import CelAnimate.Tool.PolygonErase as PolygonErase
import CelAnimate.Tool.PolygonMove as PolygonMove
import DOM
import Dict
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe.Extra as Maybe
import Round


leftBar : ModeState -> Html Msg
leftBar mode =
    div
        [ class """h-screen w-8 bg-gray-700 flex flex-col
             pointer-events-auto flex-none mr-px"""
        ]
        [ case mode of
            MorphMode state _ ->
                Html.map (\s -> SwitchMode <| MorphMode s False) <|
                    div []
                        [ toolIcon "arrows-alt" (morphToolNum state == 0) <|
                            MorphMove MorphMove.initState
                        ]

            _ ->
                text ""
        ]


topBar : ModeState -> Html Msg
topBar mode =
    case mode of
        MorphMode _ _ ->
            div [ class "flex-none" ] []

        MeshEditMode state _ mesh ->
            div [ class "flex flex-row w-full bg-gray-800 px-px pb-px" ]
                [ Html.map (\s -> SwitchMode <| MeshEditMode s False mesh) <|
                    div [ class "flex flex-row flex-auto px-4 bg-gray-700" ]
                        [ toolIcon "arrows-alt" (meshEditToolNum state == 0) <|
                            PolygonMove PolygonMove.initState
                        , toolIcon "paint-brush" (meshEditToolNum state == 1) <|
                            PolygonDraw PolygonDraw.initState
                        , toolIcon "eraser" (meshEditToolNum state == 2) <|
                            PolygonErase PolygonErase.initState
                        ]
                , div [ class """flex flex-row-reverse text-center px-4 
                             bg-gray-700""" ]
                    [ Html.button
                        [ class "bg-gray-800 hover:bg-gray-900 px-2 m-1"
                        , onClick <|
                            Batch
                                (ModifyData <| MeshEdit.finish mesh)
                                (SwitchMode <| Morph.initState)
                        ]
                        [ icon_ "check", text "Finish Mesh Editing" ]
                    ]
                ]


properties : ModeState -> ToolSettings -> Html Msg
properties mode settings =
    Html.map ToolSet <|
        div [ class """flex flex-col h-1-3 bg-gray-800 p-px
                       overflow-y-scroll""" ] <|
            div [ class "bg-gray-700 m-px p-1" ] [ text "Tool" ]
                :: (case mode of
                        MorphMode state _ ->
                            morphModeProps state settings

                        MeshEditMode state _ _ ->
                            meshEditModeProps state settings
                   )


morphModeProps : MorphToolState -> ToolSettings -> List (Html ToolSettings)
morphModeProps state settings =
    case state of
        MorphMove _ ->
            let
                update f x =
                    { settings
                        | morphMove = f settings.morphMove x
                    }
            in
            [ paramSlider "radius" 0.01 10 0.01 2 settings.morphMove.radius
                |> Html.map
                    (update <| \s x -> { s | radius = x })
            ]


meshEditModeProps :
    MeshEditToolState
    -> ToolSettings
    -> List (Html ToolSettings)
meshEditModeProps state settings =
    case state of
        PolygonMove _ ->
            let
                update f x =
                    { settings
                        | polygonMove = f settings.polygonMove x
                    }
            in
            [ paramSlider "radius" 0.01 1 0.01 2 settings.polygonMove.radius
                |> Html.map
                    (update <| \s x -> { s | radius = x })
            ]

        PolygonDraw _ ->
            let
                update f x =
                    { settings
                        | polygonDraw = f settings.polygonDraw x
                    }
            in
            [ paramSlider "radius" 0.01 1 0.01 2 settings.polygonDraw.radius
                |> Html.map
                    (update <| \s x -> { s | radius = x })
            ]

        PolygonErase _ ->
            let
                update f x =
                    { settings
                        | polygonErase = f settings.polygonErase x
                    }
            in
            [ paramSlider "radius" 0.01 1 0.01 2 settings.polygonErase.radius
                |> Html.map
                    (update <| \s x -> { s | radius = x })
            ]


paramSlider :
    String
    -> Float
    -> Float
    -> Float
    -> Int
    -> Float
    -> Html Float
paramSlider name min max step round x =
    div [ class "bg-gray-700 m-px p-1" ]
        [ p [] [ text name, text ": ", text <| Round.round round x ]
        , slider min max step round x
        ]


meshEditToolNum : MeshEditToolState -> Int
meshEditToolNum state =
    case state of
        PolygonMove _ ->
            0

        PolygonDraw _ ->
            1

        PolygonErase _ ->
            2


morphToolNum : MorphToolState -> Int
morphToolNum state =
    0


toolIcon : String -> Bool -> msg -> Html msg
toolIcon name now msg =
    Html.button
        [ class <|
            "w-8 h-8 select-none focus:outline-none text-xl select-none "
                ++ (if now then
                        "bg-teal-700"

                    else
                        "bg-gray-700"
                   )
        , onClick msg
        ]
        [ icon name ]


cursorView : Model -> Html Msg
cursorView model =
    div
        [ class "cursor-polygon-draw fixed"
        , style "top"
            (String.fromFloat (Vec2.getX model.cursor.position)
                ++ "px"
            )
        , style "left"
            (String.fromFloat (Vec2.getY model.cursor.position)
                ++ "px"
            )
        , style "width" "20px"
        , style "height" "20px"
        ]
        []


pointer : PointerEvent -> Pointer.Event -> Model -> ( Cursor, ToolMsg )
pointer pe event model =
    let
        position =
            vec2 (Tuple.first event.pointer.offsetPos)
                (Tuple.second event.pointer.offsetPos)

        cursor =
            model.cursor

        displace =
            Vec2.sub position cursor.position

        velocity =
            Vec2.add (Vec2.scale 0.5 displace)
                (Vec2.scale 0.5 cursor.velocity)

        newCursor =
            { position = position
            , displace = displace
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
            , movement = cursorVelocity model newCursor.displace
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
