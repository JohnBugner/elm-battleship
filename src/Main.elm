module Main exposing (..)

import App
import Extra
import Msg
import View

import Browser
import Task
import Time

main =
    Browser.element
        { init = \ () -> ( App.init, Task.perform Msg.SetBoard Time.now )
        , update = \ msg -> Extra.noCmd << App.update msg
        , subscriptions = App.subs
        , view = View.appView
        }
