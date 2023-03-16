module Ship exposing (..)

import Direction
import Extra
import Grid

import Dict
import Random

type Ship
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier

length : Ship -> Int
length ship =
    case ship of
        Destroyer  -> 2
        Submarine  -> 3
        Cruiser    -> 3
        Battleship -> 4
        Carrier    -> 5

abbreviation : Ship -> String
abbreviation ship =
    case ship of
        Destroyer  -> "D"
        Submarine  -> "S"
        Cruiser    -> "C"
        Battleship -> "B"
        Carrier    -> "A"

inits : List Ship
inits =
    [ Destroyer
    , Submarine
    , Cruiser
    , Battleship
    , Carrier
    ]

coordsOfShipStartedAt : Ship -> Direction.Direction -> (Int,Int) -> List (Int,Int)
coordsOfShipStartedAt ship direction (x,y) =
    let
        n : Int
        n = length ship
    in
        case direction of
            Direction.Horizontal ->
                List.map (\ xx -> (xx,y)) <|
                List.range x (x + n - 1)
            Direction.Vertical ->
                List.map (\ yy -> (x,yy)) <|
                List.range y (y + n - 1)

maybePlaceShip : Ship -> Direction.Direction -> (Int,Int) -> Grid.Grid Ship -> (Int,Int) -> Maybe (Grid.Grid Ship)
maybePlaceShip ship direction wh placedShips xy =
    if isValidStartingCoordForShip ship direction wh placedShips xy
    then
        Just <|
        List.foldl (\ xy_ -> Dict.insert xy_ ship) placedShips <|
        coordsOfShipStartedAt ship direction xy
    else Nothing

validStartingCoordsOfShip : Ship -> Direction.Direction -> (Int,Int) -> Grid.Grid Ship -> List (Int,Int)
validStartingCoordsOfShip ship direction wh placedShips =
    List.filter (isValidStartingCoordForShip ship direction wh placedShips) <|
    Extra.range2d <|
    Extra.add wh (-1,-1)

isValidStartingCoordForShip : Ship -> Direction.Direction -> (Int,Int) -> Grid.Grid Ship -> (Int,Int) -> Bool
isValidStartingCoordForShip ship direction (w,h) placedShips xy =
    let
        coordIsInBounds : (Int,Int) -> Bool
        coordIsInBounds (x,y) = (x >= 0) && (x < w) && (y >= 0) && (y < h)
        coordIsEmpty : (Int,Int) -> Bool
        coordIsEmpty xy_ = not <| Dict.member xy_ placedShips
    in
        List.all (\ xy_ -> coordIsInBounds xy_ && coordIsEmpty xy_) <|
        coordsOfShipStartedAt ship direction xy

placedShipsGen : (Int,Int) -> List Ship -> Random.Generator (Grid.Grid Ship)
placedShipsGen wh ships =
    let
        f : Ship -> Random.Generator (Grid.Grid Ship) -> Random.Generator (Grid.Grid Ship)
        f ship gen = Random.andThen (placedShipGen wh ship) gen
    in
        List.foldl f (Random.constant Dict.empty) ships

placedShipGen : (Int,Int) -> Ship -> Grid.Grid Ship -> Random.Generator (Grid.Grid Ship)
placedShipGen wh ship placedShips =
    let
        f : Direction.Direction -> Random.Generator (Grid.Grid Ship)
        f direction =
            let
                aa = validStartingCoordsOfShip ship direction wh placedShips
            in
                case aa of
                    a :: as_ ->
                        Random.map (Maybe.withDefault placedShips) <|
                        Random.map (\ xy -> maybePlaceShip ship direction wh placedShips xy) <|
                        Random.uniform a as_
                    [] -> Random.constant placedShips
    in
        Random.andThen f <|
        Direction.directionGen
