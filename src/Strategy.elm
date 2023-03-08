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

maybeShotCoord : OpenBoard.OpenBoard -> Strategy -> Maybe (Int,Int)
maybeShotCoord openBoard strategy =
    case strategy of
        -- Picks a coordinate from left to right, then top to bottom.
        Ordered ->
            List.head <|
            List.sortBy Tuple.second <|
            Set.toList <|
            OpenBoard.notShotCoords openBoard
        -- Picks a random coordinate to shoot.
        Random ->
            let
                gen : Random.Generator (Maybe (Int,Int))
                gen =
                    Random.map Tuple.first <|
                    Random.List.choose <|
                    Set.toList <|
                    OpenBoard.notShotCoords openBoard
                seed : Random.Seed
                seed = Random.initialSeed 0
            in
                Tuple.first <|
                Random.step gen seed
