module App exposing (..)

import Board
import Msg
import Ship
import Strategy
import ViewType

import Random
import Time

type alias App =
    { maybeBoard : Maybe Board.Board
    , maybeViewType : Maybe ViewType.ViewType
    , maybeStrategy : Maybe Strategy.Strategy
    , isSolving : Bool
    }

init : App
init =
    { maybeBoard = Nothing
    , maybeViewType = Just ViewType.Player
    , maybeStrategy = Just Strategy.Smart
    , isSolving = False
    }

update : Msg.Msg -> App -> App
update msg app =
    case msg of
        Msg.SetBoard time ->
            let
                size : (Int,Int)
                --size = (10,10)
                size = (8,8)
                shipTypes : List Ship.Ship
                shipTypes =
                    [ Ship.Destroyer
                    --, Ship.Submarine
                    --, Ship.Cruiser
                    --, Ship.Battleship
                    , Ship.AircraftCarrier
                    ]
                seed : Random.Seed
                seed = Random.initialSeed <| Time.posixToMillis time
            in
                { app
                | maybeBoard = Just <| Board.init size shipTypes seed
                }
        Msg.SetViewType ident -> { app | maybeViewType = ViewType.fromString ident }
        Msg.SetStrategy ident -> { app | maybeStrategy = Strategy.fromString ident }
        Msg.StartStopSolving -> { app | isSolving = not app.isSolving }
        Msg.Tick _ ->
            case ( app.maybeBoard, app.maybeStrategy ) of
                ( Just board, Just strategy ) ->
                    if app.isSolving
                    then
                        case Board.solve strategy board of
                            Just newBoard -> { app | maybeBoard = Just newBoard }
                            Nothing -> { app | isSolving = False }
                    else app
                _ -> app
        Msg.Shoot location ->
            case ( app.maybeBoard, app.maybeStrategy ) of
                ( Just board, Just Strategy.Manual ) ->
                    if Board.isSolved board
                    then app
                    else 
                        let
                            maybeNewBoard : Maybe Board.Board
                            maybeNewBoard = Board.shoot location board
                        in
                            { app | maybeBoard = maybeNewBoard }
                _ -> app
        Msg.ResetBoard ->
            case app.maybeBoard of
                Just board ->
                    { app
                    | maybeBoard = Just (Board.reset board)
                    }
                _ -> app

subs : App -> Sub Msg.Msg
subs app =
    if app.isSolving
    then Time.every (1000 / 4) Msg.Tick
    else Sub.none
