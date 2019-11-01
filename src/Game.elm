module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E
import List
import Player exposing (PlayerType(..), playerTypeDecoder)


type alias Coord =
    ( Int, Int )


type Cell
    = Empty Bool
    | Living PlayerType Bool
    | Armor PlayerType Bool


type alias Game =
    { board : Dict Coord Cell
    , currentPlayer : PlayerType
    , movesLeft : Int
    , message : String
    }


viewGame : Game -> (Coord -> msg) -> Html msg
viewGame game moveMsg =
    div [ class "game" ] (viewCells game.board moveMsg)


viewCells : Dict Coord Cell -> (Coord -> msg) -> List (Html msg)
viewCells board moveMsg =
    Dict.values (Dict.map (\coord cell -> viewCell coord cell moveMsg) board)


viewCell : Coord -> Cell -> (Coord -> msg) -> Html msg
viewCell ( i, j ) cell moveMsg =
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
    div [ style "grid-row" (String.fromInt <| i + 1), style "grid-column" (String.fromInt <| j + 1), onClick (moveMsg ( i, j )), class additionalClass ]
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


mapIncomingGame : (Game -> msg) -> msg -> (E.Value -> msg)
mapIncomingGame successMsg errMsg game =
    case decodeValue gameDecoder game of
        Ok res ->
            successMsg res

        _ ->
            let
                _ =
                    Debug.log "kek" "shit"
            in
            errMsg


gameDecoder : Decoder Game
gameDecoder =
    map4 Game
        (field "board" boardDecoder)
        (field "current_player" playerTypeDecoder)
        (field "moves_left" int)
        (field "message" string)


boardDecoder : Decoder (Dict Coord Cell)
boardDecoder =
    keyValuePairs cellDecoder
        |> andThen
            (\pairs ->
                let
                    dict =
                        List.map
                            (\( str, cell ) ->
                                case strToCoords str of
                                    Just coord ->
                                        ( coord, cell )

                                    Nothing ->
                                        ( ( 0, 0 ), cell )
                            )
                            pairs
                            |> Dict.fromList
                in
                D.succeed dict
            )


strToCoords : String -> Maybe Coord
strToCoords s =
    case String.split "," s of
        [ i, j ] ->
            case ( D.decodeString int i, D.decodeString int j ) of
                ( Ok decodedI, Ok decodedJ ) ->
                    Just ( decodedI, decodedJ )

                _ ->
                    Nothing

        _ ->
            Nothing


cellDecoder : Decoder Cell
cellDecoder =
    index 0 string
        |> andThen
            (\res ->
                case res of
                    "empty" ->
                        index 1 bool
                            |> andThen
                                (\boolRes ->
                                    D.succeed (Empty boolRes)
                                )

                    "living" ->
                        index 1 playerTypeDecoder
                            |> andThen
                                (\playerType ->
                                    index 2 bool
                                        |> andThen
                                            (\b ->
                                                D.succeed (Living playerType b)
                                            )
                                )

                    "armor" ->
                        index 1 playerTypeDecoder
                            |> andThen
                                (\playerType ->
                                    index 2 bool
                                        |> andThen
                                            (\b ->
                                                D.succeed (Armor playerType b)
                                            )
                                )

                    _ ->
                        D.fail "fail"
            )
