module View exposing (..)

import App
import Board
import Msg
import ShipType
import ShotResult
import ViewType

import Dict
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra
import Svg
import Svg.Attributes

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
                            [ Html.Attributes.value "Ordered"
                            ]
                            [ Html.text "Ordered"
                            ]
                        , Html.option
                            [ Html.Attributes.value "Random"
                            ]
                            [ Html.text "Random"
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
                , Html.div [] [ Html.text <| "Shots : " ++ (Debug.toString <| Dict.size board.shotResults) ]
                ]
        ( _, _ ) -> Html.div [] []

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
                        ( case viewType of
                            ViewType.Player -> []
                            ViewType.God    -> (List.map shipTypeView <| Dict.toList board.placedShips)
                        )
                    , Svg.g
                        []
                        (List.map shotResultView <| Dict.toList board.shotResults)
                    , Svg.g
                        []
                        ( case viewType of
                            ViewType.Player ->
                                List.map shipTypeAbbreviationView <|
                                Dict.toList <|
                                Dict.filter (\ location _ -> Dict.member location board.shotResults) board.placedShips
                            ViewType.God ->
                                List.map shipTypeAbbreviationView <|
                                Dict.toList board.placedShips
                        )
                    ]
                ]
            ]

shipTypeView : ((Int,Int), ShipType.ShipType) -> Svg.Svg Msg.Msg
shipTypeView ((x,y), shipType) =
    Svg.use
        [ Svg.Attributes.xlinkHref "#ship"
        , Svg.Attributes.x <| Debug.toString x
        , Svg.Attributes.y <| Debug.toString y
        ]
        []

shotResultView : ((Int,Int), ShotResult.ShotResult) -> Svg.Svg Msg.Msg
shotResultView ((x,y), shotResult) =
    let
        mark : String -> Svg.Svg a
        mark shotResultIdent =
            Svg.use
                [ Svg.Attributes.xlinkHref shotResultIdent
                , Svg.Attributes.x <| Debug.toString x
                , Svg.Attributes.y <| Debug.toString y
                ]
                []
    in
        case shotResult of
            ShotResult.Hit shipType -> mark "#hit"
            ShotResult.Miss         -> mark "#miss"

shipTypeAbbreviationView : ((Int,Int), ShipType.ShipType) -> Svg.Svg Msg.Msg
shipTypeAbbreviationView ((x,y), shipType) =
    Svg.text_
        [ Svg.Attributes.x <| Debug.toString <| (+) 0.5 <| toFloat x
        , Svg.Attributes.y <| Debug.toString <| (+) 0.6 <| toFloat y
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.dominantBaseline "middle"
        , Svg.Attributes.fontSize "1"
        ]
        [ Svg.text <| ShipType.abbreviation shipType
        ]
