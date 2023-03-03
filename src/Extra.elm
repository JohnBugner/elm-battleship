module Extra exposing (..)

import List.Extra

cartesianProductPairs : List a -> List a -> List (a,a)
cartesianProductPairs as1 as2 =
    let
        f : List a -> Maybe (a,a)
        f xys =
            case xys of
                x :: y :: [] -> Just (x,y)
                _ -> Nothing
    in
        List.filterMap f <|
        List.Extra.cartesianProduct [as1, as2]

allCoordsInGrid : (Int,Int) -> List (Int,Int)
allCoordsInGrid (w,h) = cartesianProductPairs (List.range 0 (w-1)) (List.range 0 (h-1))
