module Strategy exposing (..)

import Extra
import OpenBoard

import Dict
import Set

type Strategy
    = LeftToRightTopToBottom

maybeShotCoord : OpenBoard.OpenBoard -> Strategy -> Maybe (Int,Int)
maybeShotCoord openBoard strategy =
    case strategy of
        LeftToRightTopToBottom ->
            let
                a = Set.fromList <| Extra.allCoordsInGrid openBoard.size
                b = Set.fromList <| List.map Tuple.first <| Dict.toList openBoard.shotResults
                c = Set.diff a b
                d = List.sortBy Tuple.second <| Set.toList c
            in
                List.head d
