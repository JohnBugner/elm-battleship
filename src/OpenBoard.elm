module OpenBoard exposing (..)

import Grid
import ShotResult

type alias OpenBoard =
    { size : (Int,Int)
    , shotResults : Grid.Grid ShotResult.ShotResult
    }
