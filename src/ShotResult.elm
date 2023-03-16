module ShotResult exposing (..)

import Ship

type ShotResult
    = Hit Ship.Ship
    | Miss
