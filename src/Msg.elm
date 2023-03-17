module Msg exposing (..)

import Time

type Msg
    = SetBoard Time.Posix
    | SetViewType String
    | SetStrategy String
    | StartStopSolving
    | Tick Time.Posix
