module Board exposing (..)

import Grid
import Msg
import ShotResult
import Strategy
import OpenBoard

import Dict

type alias Board =
    { size : (Int,Int)
    , ships : Grid.Grid Ship
    , shotResults : Grid.Grid ShotResult.ShotResult
    }

type Ship
    = Ship

init : (Int,Int) -> Board
init size =
    { size = size
    --, ships = Dict.empty -- fix
    , ships = Dict.fromList [((5,5),Ship),((9,8),Ship),((7,9),Ship)]
    , shotResults = Dict.empty
    }

shoot : (Int,Int) -> Board -> Maybe Board
shoot shotCoord board =
    if Dict.member shotCoord board.shotResults
    then Nothing
    else
        Just 
            { board
            | shotResults =
                Dict.insert
                    shotCoord
                    (if Dict.member shotCoord board.ships then ShotResult.Hit else ShotResult.Miss)
                    board.shotResults
            }

update : Msg.Msg -> Board -> Board
update msg board =
    case msg of
        Msg.Solve -> solve Strategy.LeftToRightTopToBottom board

solve : Strategy.Strategy -> Board -> Board
solve strategy board =
    let
        maybeShotCoord : Maybe (Int,Int)
        maybeShotCoord = Strategy.maybeShotCoord (toOpenBoard board) strategy
    in
        -- Is the board already solved ?
        if isSolved board
        then board
        else
            -- Did the algorithm even fire a shot ?
            case maybeShotCoord of
                Just shotCoord ->
                    -- Has a shot already been fired at that coordinate ?
                    case shoot shotCoord board of
                        Just newBoard -> solve strategy newBoard
                        Nothing -> board
                Nothing -> board

isSolved : Board -> Bool
isSolved board = Dict.isEmpty <| Dict.diff board.ships board.shotResults

toOpenBoard : Board -> OpenBoard.OpenBoard
toOpenBoard board =
    { size = board.size
    , shotResults = board.shotResults
    }
