module App exposing (..)

import Board
import Msg
import Strategy

import Random
import Time

type alias App =
    { maybeBoard : Maybe Board.Board
    , maybeStrategy : Maybe Strategy.Strategy
    , isSolving : Bool
    }

init : App
init =
    { maybeBoard = Nothing
    , maybeStrategy = Just Strategy.Ordered
    , isSolving = False
    }

update : Msg.Msg -> App -> App
update msg app =
    case msg of
        Msg.SetBoard time ->
            let
                seed : Random.Seed
                seed = Random.initialSeed <| Time.posixToMillis time
            in
                { app
                | maybeBoard = Just <| Board.init (10,10) seed
                }
        Msg.SetStrategy ident -> { app | maybeStrategy = Strategy.fromString ident}
        Msg.Solve -> { app | isSolving = not app.isSolving}
        Msg.Tick _ ->
            if app.isSolving
            then
                case ( app.maybeBoard, app.maybeStrategy ) of
                    ( Just board, Just strategy ) ->
                        case Board.solveStep strategy board of
                            Just newBoard -> { app | maybeBoard = Just newBoard }
                            Nothing -> { app | isSolving = False }
                    _ -> app
            else app

subs : App -> Sub Msg.Msg
subs app =
    if app.isSolving
    then Time.every (1000 / 4) Msg.Tick
    else Sub.none
