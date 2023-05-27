module View exposing (..)

import App
import Board
import Extra
import Msg
import Ship
import Shot
import ViewType

import Dict
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra
import Svg
import Svg.Attributes
import Svg.Events

appView : App.App -> Html.Html Msg.Msg
appView app =
    case ( app.maybeBoard, app.maybeViewType ) of
        ( Just board, Just viewType ) ->
            Html.div
                []
                [ boardView viewType board
                , Html.div
                    []
                    [ Html.select
                        [ Html.Events.Extra.onChange Msg.SetViewType
                        , Html.Attributes.style "width" "100px"
                        , Html.Attributes.style "height" "50px"
                        ]
                        [ Html.option
                            [ Html.Attributes.value "Player"
                            ]
                            [ Html.text "Player"
                            ]
                        , Html.option
                            [ Html.Attributes.value "God"
                            ]
                            [ Html.text "God"
                            ]
                        ]
                    ]
                , Html.div
                    []
                    [ Html.select
                        [ Html.Events.Extra.onChange Msg.SetStrategy
                        , Html.Attributes.style "width" "100px"
                        , Html.Attributes.style "height" "50px"
                        ]
                        [ Html.option
                            [ Html.Attributes.value "Smart"
                            ]
                            [ Html.text "Smart"
                            ]
                        , Html.option
                            [ Html.Attributes.value "Ordered"
                            ]
                            [ Html.text "Ordered"
                            ]
                        , Html.option
                            [ Html.Attributes.value "Random"
                            ]
                            [ Html.text "Random"
                            ]
                        , Html.option
                            [ Html.Attributes.value "Manual"
                            ]
                            [ Html.text "Manual"
                            ]
                        ]
                    ]
                , Html.div
                    []
                    [ Html.button
                        [ Html.Events.onClick Msg.StartStopSolving
                        , Html.Attributes.style "width" "100px"
                        , Html.Attributes.style "height" "50px"
                        ]
                        [ Html.text (if app.isSolving then "Stop" else "Solve")
                        ]
                    ]
                , Html.div [] [ Html.text <| "Shots : " ++ (Debug.toString <| Dict.size board.placedShots) ]
                ]
        _ -> Html.div [] []

boardView : ViewType.ViewType -> Board.Board -> Html.Html Msg.Msg
boardView viewType board =
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
                        [ Svg.Attributes.id "cell"
                        , Svg.Attributes.fill "blue"
                        , Svg.Attributes.width "1"
                        , Svg.Attributes.height "1"
                        ]
                        []
                    , Svg.rect
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
                    -- Cells
                    [ Svg.g
                        []
                        (
                            List.map cellView <|
                            Extra.pairsInRange <|
                            Extra.sub board.size (1,1)
                        )
                    -- Ships
                    , Svg.g
                        []
                        ( case viewType of
                            ViewType.Player -> []
                            ViewType.God ->
                                List.map shipView <|
                                Dict.toList <|
                                board.placedShips
                        )
                    -- Shots
                    , Svg.g
                        []
                        (List.map shotView <| Dict.toList board.placedShots)
                    -- Ship Abbreviations
                    , Svg.g
                        []
                        ( case viewType of
                            ViewType.Player ->
                                List.map shipAbbreviationView <|
                                Dict.toList <|
                                Dict.filter (\ location _ -> Dict.member location board.placedShots) <|
                                board.placedShips
                            ViewType.God ->
                                List.map shipAbbreviationView <|
                                Dict.toList <|
                                board.placedShips
                        )
                    ]
                ]
            ]

cellView : (Int,Int) -> Svg.Svg Msg.Msg
cellView (x,y) =
    Svg.use
        [ Svg.Attributes.xlinkHref "#cell"
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        , Svg.Events.onClick (Msg.Shoot (x,y))
        ]
        []

shipView : ((Int,Int), Ship.Ship) -> Svg.Svg Msg.Msg
shipView ((x,y), _) =
    Svg.use
        [ Svg.Attributes.xlinkHref "#ship"
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        ]
        []

shotView : ((Int,Int), Shot.Shot) -> Svg.Svg Msg.Msg
shotView ((x,y), shot) =
    let
        shotIdent : String
        shotIdent =
            case shot of
            Shot.Hit _ -> "#hit"
            Shot.Miss  -> "#miss"
    in
        Svg.use
            [ Svg.Attributes.xlinkHref shotIdent
            , Svg.Attributes.x <| Debug.toString x
            , Svg.Attributes.y <| Debug.toString y
            ]
            []

shipAbbreviationView : ((Int,Int), Ship.Ship) -> Svg.Svg Msg.Msg
shipAbbreviationView ((x,y), ship) =
    Svg.text_
        [ Svg.Attributes.x <| Debug.toString <| (+) 0.5 <| toFloat x
        , Svg.Attributes.y <| Debug.toString <| (+) 0.6 <| toFloat y
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.dominantBaseline "middle"
        , Svg.Attributes.fontSize "1"
        ]
        [ Svg.text <| Ship.abbreviation ship
        ]
