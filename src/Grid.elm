module Grid exposing (..)

import Dict

type alias Grid a = Dict.Dict (Int,Int) a
