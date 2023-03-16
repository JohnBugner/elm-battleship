module Board exposing (..)

import Grid
import Ship
import ShipType
import ShotResult
import Strategy
import OpenBoard

import Dict
import Random

type alias Board =
    { size : (Int,Int)
    , placedShips : Grid.Grid ShipType.ShipType
    , shotResults : Grid.Grid ShotResult.ShotResult
    }

init : (Int,Int) -> List ShipType.ShipType -> Random.Seed -> Board
init size shipTypes seed =
    { size = size
    , placedShips =
        Tuple.first <|
        Random.step (Ship.placedShipTypesGen shipTypes size) seed
    , shotResults = Dict.empty
    }

shoot : (Int,Int) -> Board -> Maybe Board
shoot location board =
    -- Has a shot already been fired at that location ?
    if Dict.member location board.shotResults
    then Nothing
    else
        Just 
            { board
            | shotResults =
                let
                    shotResult : ShotResult.ShotResult
                    shotResult =
                        case Dict.get location board.placedShips of
                            Just shipType -> ShotResult.Hit shipType
                            Nothing -> ShotResult.Miss
                in
                    Dict.insert location shotResult board.shotResults
            }

solve : Strategy.Strategy -> Board -> Maybe Board
solve strategy board =
    let
        maybeShotLocation : Maybe (Int,Int)
        maybeShotLocation = Strategy.maybeShotLocation (toOpenBoard board) strategy
    in
        -- Is the board already solved ?
        if isSolved board
        then Nothing
        else
            -- Did the strategy even fire a shot ?
            case maybeShotLocation of
                Just location -> shoot location board
                Nothing -> Nothing

isSolved : Board -> Bool
isSolved board =
    Dict.isEmpty <|
    Dict.diff board.placedShips board.shotResults

toOpenBoard : Board -> OpenBoard.OpenBoard
toOpenBoard board =
    { size = board.size
    , shotResults = board.shotResults
    }
