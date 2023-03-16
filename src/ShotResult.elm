module ShotResult exposing (..)

import ShipType

type ShotResult
    = Hit ShipType.ShipType
    | Miss
