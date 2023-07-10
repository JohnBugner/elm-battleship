module Board exposing (..)

import Grid
import LiveShip
import Ship
import Shot
import Strategy
import OpenBoard

import Dict
import List.Extra
import Random

type alias Board =
    { size : (Int,Int)
    , ships : List Ship.Ship
    , placedShips : Grid.Grid Ship.Ship
    , placedShots : Grid.Grid Shot.Shot
    , matchingShipGrids : List (Grid.Grid Ship.Ship)
    }

init : (Int,Int) -> List Ship.Ship -> Random.Seed -> Board
init size ships seed =
    { size = size
    , ships = ships
    , placedShips =
        Tuple.first <|
        Random.step (LiveShip.placedShipsGen ships size) seed
    , placedShots = Dict.empty
    , matchingShipGrids = LiveShip.grids ships size
    }

reset : Board -> Board
reset board =
    { board
    | placedShots = Dict.empty
    , matchingShipGrids = LiveShip.grids board.ships board.size
    }

shoot : (Int,Int) -> Board -> Maybe Board
shoot location board =
    -- Has a shot already been fired at that location ?
    if Dict.member location board.placedShots
    then Nothing
    else
        let
            shot : Shot.Shot
            shot =
                case Dict.get location board.placedShips of
                    Just ship -> Shot.Hit ship
                    Nothing -> Shot.Miss
        in
        Just 
            { board
            | placedShots = Dict.insert location shot board.placedShots
            , matchingShipGrids = List.filter (OpenBoard.matches (location, shot)) board.matchingShipGrids
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
    , matchingShipGrids = board.matchingShipGrids
    }

shipsLeft : Board -> List Ship.Ship
shipsLeft board =
    let
        f : (Ship.Ship, List Ship.Ship) -> (Ship.Ship, Int)
        f (ship, ships) =
            let
                startCellCount = (List.length ships + 1) * Ship.length ship
                currentCellCount =
                    Dict.size <|
                    Dict.filter (\_ shot -> shot == Shot.Hit ship) board.placedShots
                currentShipCount = ceiling <| ((toFloat <| startCellCount - currentCellCount) / (toFloat <| Ship.length ship))
            in
                (ship, currentShipCount)

        ungroup : List (a, Int) -> List a
        ungroup = List.concatMap (\ (a, count) -> List.repeat count a)
    in
        ungroup <| List.map f <| (List.Extra.gatherEquals board.ships)
