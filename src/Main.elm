module Main exposing (..)

import App
import Extra
import Msg
import View

import Browser

main : Program () App.App Msg.Msg
main =
    Browser.element
        { init = Extra.noCmd << always App.init
        , update = \ msg -> Extra.noCmd << App.update msg
        , subscriptions = App.subs
        , view = View.appView
        }
