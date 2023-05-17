module Extra exposing (..)

import List.Extra

noCmd : a -> (a, Cmd b)
noCmd a = (a, Cmd.none)

add : (number1,number2) -> (number1,number2) -> (number1,number2)
add (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

-- Starts at (0,0). Excludes (w,h).
isInRange : (Int,Int) -> (Int,Int) -> Bool
isInRange (x,y) (w,h) = (x >= 0) && (x < w) && (y >= 0) && (y < h)

-- Starts at (0,0). Includes (w,h).
range2Dim : (Int,Int) -> List (Int,Int)
range2Dim (w,h) = cartesianProductPairs (List.range 0 w) (List.range 0 h)

cartesianProductPairs : List a -> List a -> List (a,a)
cartesianProductPairs as1 as2 =
    let
        f : List a -> Maybe (a,a)
        f xy =
            case xy of
                x :: y :: [] -> Just (x,y)
                _ -> Nothing
    in
        List.filterMap f <|
        List.Extra.cartesianProduct [as1, as2]
