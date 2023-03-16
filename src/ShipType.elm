module ShipType exposing (..)

type ShipType
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier

length : ShipType -> Int
length shipType =
    case shipType of
        Destroyer  -> 2
        Submarine  -> 3
        Cruiser    -> 3
        Battleship -> 4
        Carrier    -> 5

abbreviation : ShipType -> String
abbreviation shipType =
    case shipType of
        Destroyer  -> "D"
        Submarine  -> "S"
        Cruiser    -> "C"
        Battleship -> "B"
        Carrier    -> "A"
