module View exposing (..)

import App
import Board
import Dict
import Msg
import ShotResult

import Html
import Html.Events
import Svg
import Svg.Attributes

appView : App.App -> Html.Html Msg.Msg
appView app =
    Html.div
        []
        [ Html.button [ Html.Events.onClick Msg.Solve ] [ Html.text (if app.isSolving then "Stop" else "Solve") ]
        , boardView app.board
        ]

boardView : Board.Board -> Html.Html Msg.Msg
boardView board =
    let
        (w,h) = board.size
    in
        Svg.svg
            [ Svg.Attributes.viewBox "0 0 10 10"
            , Svg.Attributes.width "480"
            , Svg.Attributes.height "480"
            ]
            [ Svg.g
                []
                [ Svg.defs
                    []
                    [ Svg.rect
                        [ Svg.Attributes.id "ship"
                        , Svg.Attributes.fill "gray"
                        , Svg.Attributes.width "1"
                        , Svg.Attributes.height "1"
                        ]
                        []
                    , Svg.rect
                        [ Svg.Attributes.id "hit"
                        , Svg.Attributes.fill "red"
                        , Svg.Attributes.width "1"
                        , Svg.Attributes.height "1"
                        , Svg.Attributes.rx "1"
                        ]
                        []
                    , Svg.rect
                        [ Svg.Attributes.id "miss"
                        , Svg.Attributes.fill "white"
                        , Svg.Attributes.width "1"
                        , Svg.Attributes.height "1"
                        , Svg.Attributes.rx "1"
                        ]
                        []
                    ]
                , Svg.g
                    []
                    [ Svg.rect
                        [ Svg.Attributes.fill "blue"
                        , Svg.Attributes.width <| Debug.toString w
                        , Svg.Attributes.height <| Debug.toString h
                        ]
                        []
                    , Svg.g
                        []
                        (List.map shipView <| Dict.toList board.ships)
                    , Svg.g
                        []
                        (List.map shotResultView <| Dict.toList board.shotResults)
                    ]
                ]
            ]

shipView : ((Int,Int), Board.Ship) -> Svg.Svg Msg.Msg
shipView ((x,y),ship) =
    Svg.use
        [ Svg.Attributes.xlinkHref "#ship"
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        ]
        []

shotResultView : ((Int,Int), ShotResult.ShotResult) -> Svg.Svg Msg.Msg
shotResultView ((x,y),shotResult) =
    Svg.use
        [ Svg.Attributes.xlinkHref (ShotResult.ident shotResult)
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        ]
        []
