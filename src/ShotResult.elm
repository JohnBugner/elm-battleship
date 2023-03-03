module ShotResult exposing (..)

type ShotResult
    = Hit
    | Miss

ident : ShotResult -> String
ident shotResult =
    case shotResult of
        Hit -> "#hit"
        Miss -> "#miss"
