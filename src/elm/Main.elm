module Main exposing (..)

import Browser
import CelAnimate.Editor exposing (..)
import CelAnimate.Editor.Model exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }
