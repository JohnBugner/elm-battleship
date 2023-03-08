module Msg exposing (..)

import Strategy

import Time

type Msg
    = SetStrategy (Maybe Strategy.Strategy)
    | Solve
    | Tick Time.Posix
