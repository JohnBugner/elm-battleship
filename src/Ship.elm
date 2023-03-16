module Ship exposing (..)

import Direction
import Extra
import Grid
import ShipType

import Dict
import Random

type alias Ship =
    { shipType : ShipType.ShipType
    , direction : Direction.Direction
    , location : (Int,Int)
    }

locations : Ship -> List (Int,Int)
locations ship =
    let
        n : Int
        n = ShipType.length ship.shipType
        (x,y) = ship.location
    in
        case ship.direction of
            Direction.Horizontal ->
                List.map (\ x_ -> (x_,y)) <|
                List.range x (x + n - 1)
            Direction.Vertical ->
                List.map (\ y_ -> (x,y_)) <|
                List.range y (y + n - 1)

maybePlaceShip : Ship -> (Int,Int) -> Grid.Grid ShipType.ShipType -> Maybe (Grid.Grid ShipType.ShipType)
maybePlaceShip ship size placedShips =
    if isValidStartingLocationForShip ship size placedShips
    then
        Just <|
        List.foldl (\ location -> Dict.insert location ship.shipType) placedShips <|
        locations ship
    else Nothing

validStartingLocationsForShipTypeAndDirection : ShipType.ShipType -> Direction.Direction -> (Int,Int) -> Grid.Grid ShipType.ShipType -> List (Int,Int)
validStartingLocationsForShipTypeAndDirection shipType direction size placedShips =
    List.filter (\ location -> isValidStartingLocationForShip (Ship shipType direction location) size placedShips) <|
    Extra.range2d <|
    Extra.add size (-1,-1)

isValidStartingLocationForShip : Ship -> (Int,Int) -> Grid.Grid ShipType.ShipType -> Bool
isValidStartingLocationForShip ship (w,h) placedShips =
    let
        locationIsInBounds : (Int,Int) -> Bool
        locationIsInBounds (x,y) = (x >= 0) && (x < w) && (y >= 0) && (y < h)
        locationIsEmpty : (Int,Int) -> Bool
        locationIsEmpty location = not <| Dict.member location placedShips
    in
        List.all (\ location -> locationIsInBounds location && locationIsEmpty location) <|
        locations ship

placedShipTypesGen : List ShipType.ShipType -> (Int,Int) -> Random.Generator (Grid.Grid ShipType.ShipType)
placedShipTypesGen shipTypes size =
    let
        f : ShipType.ShipType -> Random.Generator (Grid.Grid ShipType.ShipType) -> Random.Generator (Grid.Grid ShipType.ShipType)
        f shipType gen = Random.andThen (placedShipTypeGen shipType size) gen
    in
        List.foldl f (Random.constant Dict.empty) shipTypes

placedShipTypeGen : ShipType.ShipType -> (Int,Int) -> Grid.Grid ShipType.ShipType -> Random.Generator (Grid.Grid ShipType.ShipType)
placedShipTypeGen shipType size placedShips =
    let
        f : Direction.Direction -> Random.Generator (Grid.Grid ShipType.ShipType)
        f direction =
            case validStartingLocationsForShipTypeAndDirection shipType direction size placedShips of
                h :: t ->
                    Random.map (Maybe.withDefault placedShips) <|
                    Random.map (\ location -> maybePlaceShip (Ship shipType direction location) size placedShips) <|
                    Random.uniform h t
                [] -> Random.constant placedShips
    in
        Random.andThen f Direction.directionGen
