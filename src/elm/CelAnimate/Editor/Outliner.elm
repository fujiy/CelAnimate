module CelAnimate.Editor.Outliner exposing (..)

import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import Html exposing (Html, div, node, p, span, text)
import Html.Attributes exposing (class, selected)
import Html.Events exposing (onClick)


dataTree : Data -> Int -> Html Msg
dataTree data selection =
    node "tree-group"
        [ class "data-tree fixed flex items-center select-none m-1 text-base "
        ]
    <|
        div [ class "group" ]
            [ icon "expand_more"
            , span [ class "bg-gray-700 shadow-lg px-1" ] [ text data.path ]
            , node "context-menu"
                [ class
                    "bg-gray-700 shadow-xl w-32"
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
        [ class "ml-4"
        , selected isSelected
        , onSelected (\_ -> DataTree <| SelectCel i)
        ]
        [ icon "layers"
        , span
            [ class <|
                "shadow-lg px-1 "
                    ++ (if isSelected then
                            "bg-teal-700"

                        else
                            "bg-gray-700"
                       )
            ]
            [ text cel.name ]
        ]
