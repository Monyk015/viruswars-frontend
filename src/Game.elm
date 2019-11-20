module Game exposing (..)

import Dict exposing (Dict)
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


mapIncomingGame : (Game -> msg) -> msg -> (E.Value -> msg)
mapIncomingGame successMsg errMsg game =
    case decodeValue gameDecoder game of
        Ok res ->
            successMsg res

        _ ->
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
