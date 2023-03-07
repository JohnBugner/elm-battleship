module View exposing (..)

import App
import Board
import Dict
import Msg
import Ship
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
            [ Svg.Attributes.viewBox <| String.join " " <| List.map Debug.toString [0,0,w,h]
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
                        (List.map shipView <| Dict.toList board.placedShips)
                    , Svg.g
                        []
                        (List.map shotResultView <| Dict.toList board.shotResults)
                    ]
                ]
            ]

shipView : ((Int,Int), Ship.Ship) -> Svg.Svg Msg.Msg
shipView ((x,y),ship) =
    Svg.use
        [ Svg.Attributes.xlinkHref "#ship"
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        ]
        []

shotResultView : ((Int,Int), ShotResult.ShotResult) -> Svg.Svg Msg.Msg
shotResultView ((x,y),shotResult) =
    let
        ident : ShotResult.ShotResult -> String
        ident shotResult_ =
            case shotResult_ of
                ShotResult.Hit -> "#hit"
                ShotResult.Miss -> "#miss"
    in
        Svg.use
            [ Svg.Attributes.xlinkHref <| ident shotResult
            , Svg.Attributes.x <| Debug.toString x
            , Svg.Attributes.y <| Debug.toString y
            ]
            []
