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
    }

allLocations : OpenBoard -> Set.Set (Int,Int)
allLocations openBoard =
    Set.fromList <|
    Extra.range2Dim <|
    Extra.add openBoard.size (-1,-1)

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
                        List.map amountOfBoardsThatMatch <|
                        List.map (\ ship -> { openBoard | placedShots = Dict.insert location (Shot.Hit ship) openBoard.placedShots }) <|
                        openBoard.ships
                    b : Float
                    b =
                        toFloat <|
                        amountOfBoardsThatMatch openBoard
                in
                    a / b
            else 0

amountOfBoardsThatMatch : OpenBoard -> Int
amountOfBoardsThatMatch openBoard =
    List.length <|
    List.filter (matches openBoard.placedShots) <|
    LiveShip.grids openBoard.ships openBoard.size

matches : Grid.Grid Shot.Shot -> Grid.Grid Ship.Ship -> Bool
matches placedShots placedShips =
    let
        locationMatches : ((Int,Int), Shot.Shot) -> Bool
        locationMatches (location, shot) =
            case ( shot, Dict.get location placedShips ) of
                ( Shot.Hit a, Just b  ) -> a == b
                ( Shot.Hit _, Nothing ) -> False
                ( Shot.Miss , Just _  ) -> False
                ( Shot.Miss , Nothing ) -> True
    in
        List.all locationMatches <|
        Dict.toList <|
        placedShots
