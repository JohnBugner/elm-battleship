module LiveShip exposing (..)

import Direction
import Extra
import Grid
import Ship

import Dict
import Random

type alias LiveShip =
    { ship : Ship.Ship
    , direction : Direction.Direction
    , startingLocation : (Int,Int)
    }

locations : LiveShip -> List (Int,Int)
locations liveShip =
    let
        n : Int
        n = Ship.length liveShip.ship
        (x,y) = liveShip.startingLocation
    in
        case liveShip.direction of
            Direction.Horizontal ->
                List.map (\ x_ -> (x_,y)) <|
                List.range x (x + n - 1)
            Direction.Vertical ->
                List.map (\ y_ -> (x,y_)) <|
                List.range y (y + n - 1)

maybePlaceShip : LiveShip -> (Int,Int) -> Grid.Grid Ship.Ship -> Maybe (Grid.Grid Ship.Ship)
maybePlaceShip liveShip size placedShips =
    if isValidStartingLocation liveShip size placedShips
    then
        Just <|
        List.foldl (\ location -> Dict.insert location liveShip.ship) placedShips <|
        locations liveShip
    else Nothing

validStartingLocations : Ship.Ship -> Direction.Direction -> (Int,Int) -> Grid.Grid Ship.Ship -> List (Int,Int)
validStartingLocations ship direction size placedShips =
    List.filter (\ location -> isValidStartingLocation (LiveShip ship direction location) size placedShips) <|
    Extra.pairsInRange <|
    Extra.sub size (1,1)

isValidStartingLocation : LiveShip -> (Int,Int) -> Grid.Grid Ship.Ship -> Bool
isValidStartingLocation liveShip size placedShips =
    let
        locationIsEmpty : (Int,Int) -> Bool
        locationIsEmpty location = not <| Dict.member location placedShips
    in
        List.all (\ location -> Extra.isInRange location size && locationIsEmpty location) <|
        locations liveShip

placedShipsGen : List Ship.Ship -> (Int,Int) -> Random.Generator (Grid.Grid Ship.Ship)
placedShipsGen ships size =
    let
        f : Ship.Ship -> Random.Generator (Grid.Grid Ship.Ship) -> Random.Generator (Grid.Grid Ship.Ship)
        f ship gen = Random.andThen (placedShipGen ship size) gen
    in
        List.foldl f (Random.constant Dict.empty) ships

placedShipGen : Ship.Ship -> (Int,Int) -> Grid.Grid Ship.Ship -> Random.Generator (Grid.Grid Ship.Ship)
placedShipGen ship size placedShips =
    let
        f : Direction.Direction -> Random.Generator (Grid.Grid Ship.Ship)
        f direction =
            case validStartingLocations ship direction size placedShips of
                h :: t ->
                    Random.map (Maybe.withDefault placedShips) <|
                    Random.map (\ location -> maybePlaceShip (LiveShip ship direction location) size placedShips) <|
                    Random.uniform h t
                [] -> Random.constant placedShips
    in
        Random.andThen f Direction.gen

grids : List Ship.Ship -> (Int,Int) -> List (Grid.Grid Ship.Ship)
grids ships size =
    let
        f : Ship.Ship -> List (Dict.Dict (Int,Int) Ship.Ship) -> List (Dict.Dict (Int,Int) Ship.Ship)
        f ship dicts = List.concatMap (g ship) dicts
        g : Ship.Ship -> Dict.Dict (Int,Int) Ship.Ship -> List (Dict.Dict (Int,Int) Ship.Ship)
        g ship dict =
            (
                List.filterMap (\ location -> maybePlaceShip (LiveShip ship Direction.Horizontal location) size dict ) <|
                validStartingLocations ship Direction.Horizontal size dict
            ) ++
            (
                List.filterMap (\ location -> maybePlaceShip (LiveShip ship Direction.Vertical location) size dict) <|
                validStartingLocations ship Direction.Vertical size dict
            )
    in
        List.foldl f [Dict.empty] ships
