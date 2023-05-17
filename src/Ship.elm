module Ship exposing (..)

type Ship
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | AircraftCarrier

length : Ship -> Int
length ship =
    case ship of
        Destroyer       -> 2
        Submarine       -> 3
        Cruiser         -> 3
        Battleship      -> 4
        AircraftCarrier -> 5

abbreviation : Ship -> String
abbreviation ship =
    case ship of
        Destroyer       -> "D"
        Submarine       -> "S"
        Cruiser         -> "C"
        Battleship      -> "B"
        AircraftCarrier -> "A"
