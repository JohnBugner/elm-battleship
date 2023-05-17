module Shot exposing (..)

import Ship

type Shot
    = Hit Ship.Ship
    | Miss
