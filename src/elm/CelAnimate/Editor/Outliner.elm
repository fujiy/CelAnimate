module CelAnimate.Editor.Outliner exposing (..)

import Array
import Array.Extra as Array
import CelAnimate.Data exposing (..)
import CelAnimate.Editor.Model exposing (..)
import CelAnimate.Html exposing (..)
import Html exposing (Html, div, input, label, node, p, span, text)
import Html.Attributes exposing (class, selected, type_)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)



-- view : Data -> DataSelection -> Html Msg
-- view = lazy2 view_


view : Data -> DataSelection -> Html Msg
view data selection =
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
                    , onClick <| ModifyData newCel
                    ]
                    [ text "New Cel" ]
                ]
            ]
            :: Array.indexedMapToList
                (\i cel -> celView selection i cel)
                data.cels


celView : DataSelection -> Int -> Cel -> Html Msg
celView selection i cel =
    node "tree-item"
        [ class <|
            "ml-4 pointer-events-auto "
                ++ (if selection.cel == i then
                        "bg-teal-700"

                    else
                        ""
                   )
        , selected <| selection.cel == i
        , onSelected (\_ -> SelectData <| selectCel i selection)
        ]
        [ span [] [ icon "image" ]
        , span
            []
            [ text cel.name ]
        ]


newCel : Data -> Data
newCel data =
    let
        cel =
            { zeroCel
                | name = "cel" ++ String.fromInt (Array.length data.cels)
            }
    in
    { data | cels = Array.push cel data.cels }


selectCel : Int -> DataSelection -> DataSelection
selectCel i selection =
    { selection | cel = i }
