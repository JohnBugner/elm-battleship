module Direction exposing (..)

import Random

type Direction
    = Horizontal
    | Vertical

directionGen : Random.Generator Direction
directionGen = Random.uniform Horizontal [Vertical]
