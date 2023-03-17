module Direction exposing (..)

import Random

type Direction
    = Horizontal
    | Vertical

gen : Random.Generator Direction
gen = Random.uniform Horizontal [Vertical]
