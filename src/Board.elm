module Board exposing (..)

import Grid
import LiveShip
import Ship
import Shot
import Strategy
import OpenBoard

import Dict
import Random

type alias Board =
    { size : (Int,Int)
    , ships : List Ship.Ship
    , placedShips : Grid.Grid Ship.Ship
    , placedShots : Grid.Grid Shot.Shot
    }

init : (Int,Int) -> List Ship.Ship -> Random.Seed -> Board
init size ships seed =
    { size = size
    , ships = ships
    , placedShips =
        Tuple.first <|
        Random.step (LiveShip.placedShipsGen ships size) seed
    , placedShots = Dict.empty
    }

shoot : (Int,Int) -> Board -> Maybe Board
shoot location board =
    -- Has a shot already been fired at that location ?
    if Dict.member location board.placedShots
    then Nothing
    else
        Just 
            { board
            | placedShots =
                let
                    shot : Shot.Shot
                    shot =
                        case Dict.get location board.placedShips of
                            Just ship -> Shot.Hit ship
                            Nothing -> Shot.Miss
                in
                    Dict.insert location shot board.placedShots
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
    Dict.diff board.placedShips board.placedShots

toOpenBoard : Board -> OpenBoard.OpenBoard
toOpenBoard board =
    { size = board.size
    , ships = board.ships
    , placedShots = board.placedShots
    }
