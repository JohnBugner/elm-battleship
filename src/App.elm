module App exposing (..)

import Board
import Msg
import Strategy

import Json.Decode
import Random
import Time

type alias App =
    { board : Board.Board
    , isSolving : Bool
    , maybeStrategy : Maybe Strategy.Strategy
    }

init : Json.Decode.Value -> App
init value =
    let
        seed : Random.Seed
        seed =
            Random.initialSeed <|
            Result.withDefault 0 <|
            Json.Decode.decodeValue Json.Decode.int value
    in
        { board = Board.init (10,10) seed
        , isSolving = False
        , maybeStrategy = Just Strategy.Ordered
        }

update : Msg.Msg -> App -> App
update msg app =
    case msg of
        Msg.SetStrategy maybeStrategy -> { app | maybeStrategy = maybeStrategy}
        Msg.Solve -> { app | isSolving = not app.isSolving}
        Msg.Tick _ ->
            if app.isSolving
            then
                case app.maybeStrategy of
                    Just strategy ->
                        case Board.solveStep strategy app.board of
                            Just newBoard -> { app | board = newBoard }
                            Nothing -> { app | isSolving = False }
                    Nothing -> app
            else app

subs : App -> Sub Msg.Msg
subs app =
    if app.isSolving
    then Time.every (1000 / 4) Msg.Tick
    else Sub.none
