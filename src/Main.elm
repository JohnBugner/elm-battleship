module Main exposing (..)

import Board
import View

import Browser

main =
    Browser.sandbox
        { init = Board.init (10,10)
        , update = Board.update
        , view = View.boardView
        }
