module Strategy exposing (..)

import OpenBoard

import Random
import Random.List
import Set

type Strategy
    = Smart
    | Ordered
    | Random

fromString : String -> Maybe Strategy
fromString s =
    case s of
        "Smart"   -> Just Smart
        "Ordered" -> Just Ordered
        "Random"  -> Just Random
        _         -> Nothing

maybeShotLocation : OpenBoard.OpenBoard -> Strategy -> Maybe (Int,Int)
maybeShotLocation openBoard strategy =
    case strategy of
        -- Picks a location based on which is the most likely to have a ship.
        Smart ->
            List.head <|
            List.sortBy (\ location -> 1 - (OpenBoard.probThatLocationHasShip location openBoard)) <|
            Set.toList <|
            OpenBoard.notShotLocations openBoard
        -- Picks a location from top to bottom, then left to right.
        Ordered ->
            List.head <|
            Set.toList <|
            OpenBoard.notShotLocations openBoard
        -- Picks a random location to shoot.
        Random ->
            let
                gen : Random.Generator (Maybe (Int,Int))
                gen =
                    Random.map Tuple.first <|
                    Random.List.choose <|
                    Set.toList <|
                    OpenBoard.notShotLocations openBoard
                seed : Random.Seed
                seed = Random.initialSeed 0
            in
                Tuple.first <|
                Random.step gen seed
