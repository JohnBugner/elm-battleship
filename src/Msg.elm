module Msg exposing (..)

import Time

type Msg
    = SetBoard Time.Posix
    | SetStrategy String
    | StartStopSolving
    | Tick Time.Posix
