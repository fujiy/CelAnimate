module CelAnimate.Editor.Outliner exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import Html exposing (Html, div, input, label, node, p, span, text)
import Html.Attributes exposing (class, selected, type_)
import Html.Events exposing (onClick)
import Set


dataTree : Data -> Int -> Html Msg
dataTree data selection =
    node "tree-group"
        [ class <|
            "data-tree flex items-center select-none p-1 text-base "
                ++ "flex-grow-0 flex-shrink-0 bg-gray-800 z-10 "
                ++ "overflow-visible"
        ]
    <|
        div [ class "group pointer-events-auto overflow-visible" ]
            [ icon "chevron-down"
            , span [ class "shadow-lg px-1" ] [ text data.path ]
            , node "context-menu"
                [ class
                    "bg-gray-700 shadow-xl w-32 "
                ]
                [ p
                    [ class "hover:bg-gray-800 p-1"
                    , onClick <| DataTree (NewCel 0)
                    ]
                    [ text "New Cel" ]
                ]
            ]
            :: Array.indexedMapToList
                (\i d -> dataTreeCel i (i == selection) d)
                data.cels


dataTreeCel : Int -> Bool -> Cel -> Html Msg
dataTreeCel i isSelected cel =
    node "tree-item"
        [ class <|
            "ml-4 pointer-events-auto "
                ++ (if isSelected then
                        "bg-teal-700"

                    else
                        ""
                   )
        , selected isSelected
        , onSelected (\_ -> DataTree <| SelectCel i)
        ]
        [ span [] [ icon "image" ]
        , span
            []
            [ text cel.name ]
        ]
