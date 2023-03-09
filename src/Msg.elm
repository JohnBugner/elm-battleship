module Msg exposing (..)

import Strategy

import Time

type Msg
    = SetBoard Time.Posix
    | SetStrategy String
    | Solve
    | Tick Time.Posix
