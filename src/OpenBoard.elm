module OpenBoard exposing (..)

import Extra
import Grid
import LiveShip
import Ship
import Shot

import Dict
import Set

type alias OpenBoard =
    { size : (Int,Int)
    , ships : List Ship.Ship
    , placedShots : Grid.Grid Shot.Shot
    , matchingShipGrids : List (Grid.Grid Ship.Ship)
    }

allLocations : OpenBoard -> Set.Set (Int,Int)
allLocations openBoard =
    Set.fromList <|
    Extra.pairsInRange <|
    Extra.sub openBoard.size (1,1)

shotLocations : OpenBoard -> Set.Set (Int,Int)
shotLocations openBoard =
    Set.fromList <|
    List.map Tuple.first <|
    Dict.toList <|
    openBoard.placedShots

notShotLocations : OpenBoard -> Set.Set (Int,Int)
notShotLocations openBoard = Set.diff (allLocations openBoard) (shotLocations openBoard)

probThatLocationHasShip : (Int,Int) -> OpenBoard -> Float
probThatLocationHasShip location openBoard =
    case Dict.get location openBoard.placedShots of
        Just (Shot.Hit _) -> 1
        Just Shot.Miss -> 0
        Nothing ->
            if Extra.isInRange location openBoard.size
            then
                let
                    a : Float
                    a =
                        toFloat <|
                        List.sum <|
                        List.map List.length <|
                        List.map .matchingShipGrids <|
                        List.filterMap (\ ship -> shoot location (Shot.Hit ship) openBoard) <|
                        openBoard.ships
                    b : Float
                    b =
                        toFloat <|
                        List.length <|
                        openBoard.matchingShipGrids
                in
                    a / b
            else 0

-- fix
shoot : (Int,Int) -> Shot.Shot -> OpenBoard -> Maybe OpenBoard
shoot location shot openBoard =
    -- Has a shot already been fired at that location ?
    if Dict.member location openBoard.placedShots
    then Nothing
    else
        Just 
            { openBoard
            | placedShots = Dict.insert location shot openBoard.placedShots
            , matchingShipGrids = List.filter (matches (location, shot)) openBoard.matchingShipGrids
            }

matches : ((Int,Int), Shot.Shot) -> Grid.Grid Ship.Ship -> Bool
matches (location, shot) placedShips =
    case ( shot, Dict.get location placedShips ) of
        ( Shot.Hit a, Just b  ) -> a == b
        ( Shot.Hit _, Nothing ) -> False
        ( Shot.Miss , Just _  ) -> False
        ( Shot.Miss , Nothing ) -> True
