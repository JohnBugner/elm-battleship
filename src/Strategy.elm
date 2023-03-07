module Strategy exposing (..)

import OpenBoard

import Set

type Strategy
    = LeftToRightTopToBottom

maybeShotCoord : OpenBoard.OpenBoard -> Strategy -> Maybe (Int,Int)
maybeShotCoord openBoard strategy =
    case strategy of
        LeftToRightTopToBottom ->
            List.head <|
            List.sortBy Tuple.second <|
            Set.toList <|
            OpenBoard.notShotCoords openBoard
