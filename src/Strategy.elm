module Strategy exposing (..)

import OpenBoard

import Random
import Random.List
import Set

type Strategy
    = Ordered
    | Random

fromString : String -> Maybe Strategy
fromString s =
    case s of
        "Ordered" -> Just Ordered
        "Random" -> Just Random
        _ -> Nothing

maybeShotLocation : OpenBoard.OpenBoard -> Strategy -> Maybe (Int,Int)
maybeShotLocation openBoard strategy =
    case strategy of
        -- Picks a location from left to right, then top to bottom.
        Ordered ->
            List.head <|
            List.sortBy Tuple.second <|
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
