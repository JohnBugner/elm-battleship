module App exposing (..)

import Board
import Msg
import Strategy

import Time

type alias App =
    { board : Board.Board
    , isSolving : Bool
    }

init : App
init =
    { board = Board.init (10,10)
    , isSolving = False
    }

update : Msg.Msg -> App -> App
update msg app =
    case msg of
        Msg.Solve -> { app | isSolving = not app.isSolving }
        Msg.Tick _ ->
            if app.isSolving
            then
                case Board.solve Strategy.LeftToRightTopToBottom app.board of
                    Just newBoard -> { app | board = newBoard}
                    Nothing -> { app | isSolving = False}
            else app

subs : App -> Sub Msg.Msg
subs app =
    if app.isSolving
    then Time.every (1000 / 4) Msg.Tick
    else Sub.none
