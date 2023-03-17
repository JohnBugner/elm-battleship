module OpenBoard exposing (..)

import Extra
import Grid
import ShotResult

import Dict
import Set

type alias OpenBoard =
    { size : (Int,Int)
    , shotResults : Grid.Grid ShotResult.ShotResult
    }

notShotLocations : OpenBoard -> Set.Set (Int,Int)
notShotLocations openBoard =
    let
        a : Set.Set (Int,Int)
        a =
            Set.fromList <|
            Extra.range2Dim <|
            Extra.add openBoard.size (-1,-1)
        b : Set.Set (Int,Int)
        b =
            Set.fromList <|
            List.map Tuple.first <|
            Dict.toList <|
            openBoard.shotResults
    in
        Set.diff a b
