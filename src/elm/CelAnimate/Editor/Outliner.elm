module CelAnimate.Editor.Outliner exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import Html exposing (Attribute, Html, div, input, label, node, p, span, text)
import Html.Attributes exposing (class, selected, type_)
import Html.Events exposing (onClick, onMouseUp)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Maybe.Extra as Maybe


view : Selection -> Data -> Html Msg
view selection data =
    node "tree-group"
        [ class """flex flex-row items-center select-none 
                   h-2-3 bg-gray-800 z-10 
                   overflow-y-scroll overflow-x-visible"""
        ]
    <|
        div
            [ class "group pointer-events-auto overflow-visible"
            ]
            [ icon_ "chevron-down"
            , span [ class "shadow-lg" ] [ text data.name ]
            , node "context-menu"
                [ class
                    "bg-gray-700 shadow-xl w-32 "
                ]
                [ contextMenuItem "New Part" <| ModifyData newPart
                ]
            ]
            :: Array.indexedMapToList
                (\i part -> partView selection (Path i -1) part)
                data.parts


partView : Selection -> Path -> Part -> Html Msg
partView selection this part =
    node "tree-group"
        [ class <|
            """flex items-center select-none m-px 
               flex-grow-0 flex-shrink-0 z-10 
               "overflow-visible"""
        , boolAttr "selected" <| matchPart selection this
        , onSelected <| \_ -> SelectData this
        ]
    <|
        div
            [ class "group pointer-events-auto overflow-visible"
            , selectionColor <| matchPart selection this
            ]
            [ icon_ "chevron-down"
            , span
                [ class <| "pointer-events-auto" ]
                [ text part.name ]
            , node "context-menu"
                [ class
                    "bg-gray-700 shadow-xl w-32 "
                ]
                [ contextMenuItem "New Cel" <|
                    ModifyData <|
                        \_ -> newCel this
                , contextMenuItem "New Part" <|
                    ModifyData <|
                        \_ -> newPart this
                , contextMenuItem "Delete Part" <|
                    ModifyData <|
                        \_ -> deletePart this
                ]
            ]
            :: Array.indexedMapToList
                (\k cel ->
                    celView selection
                        { this | cel = k }
                        cel
                )
                part.cels


celView : Selection -> Selection -> Cel -> Html Msg
celView selection this cel =
    node "tree-item"
        [ class "pl-4 my-px pointer-events-auto "
        , selectionColor <| matchCel selection this
        , selected <| matchCel selection this
        , onSelected (\_ -> SelectData this)
        ]
        [ span
            [ class "px-1" ]
            [ text cel.name ]
        , node "context-menu"
            [ class
                "bg-gray-700 shadow-xl w-32 "
            ]
            [ contextMenuItem "New Cel" <|
                ModifyData <|
                    \_ -> newCel this
            , contextMenuItem "Delete Cel" <|
                ModifyData <|
                    \_ -> deleteCel this
            ]
        ]


contextMenuItem : String -> Msg -> Html Msg
contextMenuItem title msg =
    p
        [ class "hover:bg-gray-800 p-1"
        , onClick msg
        ]
        [ text title ]


selectionColor : Bool -> Attribute msg
selectionColor selected =
    class
        (if selected then
            "bg-teal-700"

         else
            "bg-gray-700"
        )
