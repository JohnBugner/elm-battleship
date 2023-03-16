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
shoot shotCoord board =
    -- Has a shot already been fired at that coordinate ?
    if Dict.member shotCoord board.shotResults
    then Nothing
    else
        Just 
            { board
            | shotResults =
                let
                    shotResult : ShotResult.ShotResult
                    shotResult =
                        case Dict.get shotCoord board.placedShips of
                            Just shipType -> ShotResult.Hit shipType
                            Nothing -> ShotResult.Miss
                in
                    Dict.insert shotCoord shotResult board.shotResults
            }

solveStep : Strategy.Strategy -> Board -> Maybe Board
solveStep strategy board =
    let
        maybeShotCoord : Maybe (Int,Int)
        maybeShotCoord = Strategy.maybeShotCoord (toOpenBoard board) strategy
    in
        -- Is the board already solved ?
        if isSolved board
        then Nothing
        else
            -- Did the strategy even fire a shot ?
            case maybeShotCoord of
                Just shotCoord -> shoot shotCoord board
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
