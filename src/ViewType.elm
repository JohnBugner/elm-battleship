module ViewType exposing (..)

type ViewType
    = Player
    | God

fromString : String -> Maybe ViewType
fromString s =
    case s of
        "Player" -> Just Player
        "God" -> Just God
        _ -> Nothing
