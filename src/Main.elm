module Main exposing (..)

import App
import Extra
import View

import Browser

main =
    Browser.element
        { init = Extra.noCmd << (\ value -> App.init value)
        , update = \ msg -> Extra.noCmd << App.update msg
        , subscriptions = App.subs
        , view = View.appView
        }
