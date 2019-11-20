module GamePage exposing (..)

import Dict exposing (Dict)
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E
import List
import Model exposing (..)
import Player exposing (PlayerType(..), playerTypeDecoder)


viewGame : Game -> Html Msg
viewGame game =
    div [ class "game" ] (viewCells game.board)


viewCells : Dict Coord Cell -> List (Html Msg)
viewCells board =
    Dict.values (Dict.map (\coord cell -> viewCell coord cell) board)


viewCell : Coord -> Cell -> Html Msg
viewCell ( i, j ) cell =
    let
        ( additionalClass, innerCell ) =
            case cell of
                Empty available ->
                    ( viewAvailable available, Html.i [] [] )

                Living Player1 available ->
                    ( viewAvailable available, Html.i [ class "fas fa-times player-1-cell" ] [] )

                Living Player2 available ->
                    ( viewAvailable available, Html.i [ class "fas fa-times player-2-cell" ] [] )

                Armor Player1 connected ->
                    ( viewConnected connected, Html.i [ class "fas fa-square player-1-armor" ] [] )

                Armor Player2 connected ->
                    ( viewConnected connected, Html.i [ class "fas fa-square player-2-armor" ] [] )
    in
    div [ style "grid-row" (String.fromInt <| i + 1), style "grid-column" (String.fromInt <| j + 1), onClick (Move ( i, j )), class additionalClass ]
        [ innerCell ]


viewAvailable : Bool -> String
viewAvailable bool =
    if bool then
        "available"

    else
        ""


viewConnected : Bool -> String
viewConnected bool =
    if bool then
        "connected"

    else
        ""
