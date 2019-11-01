module Player exposing (..)

import Json.Decode as D exposing (..)
import Json.Encode as E


type PlayerType
    = Player1
    | Player2


type alias Player =
    { id : String
    , player : PlayerType
    }


viewPlayerType : Player -> String
viewPlayerType player =
    case player.player of
        Player1 ->
            "player_1"

        Player2 ->
            "player_2"


playerDecoder : Decoder Player
playerDecoder =
    D.map2 Player
        (D.field "id" D.string)
        (D.field "player" playerTypeDecoder)


playerTypeDecoder : Decoder PlayerType
playerTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "player_1" ->
                        D.succeed Player1

                    "player_2" ->
                        D.succeed Player2

                    otherwise ->
                        D.fail <| "Unknown player " ++ otherwise
            )


decodePlayer : D.Value -> Maybe Player
decodePlayer val =
    case D.decodeValue playerDecoder val of
        Ok player ->
            Just player

        Err errorMessage ->
            let
                _ =
                    Debug.log "error" errorMessage
            in
            Nothing


mapIncomingPlayer : (Player -> msg) -> msg -> D.Value -> msg
mapIncomingPlayer msgSuccess msgErr json =
    let
        _ =
            Debug.log "kek" json
    in
    case D.decodeValue playerDecoder json of
        Ok player ->
            let
                _ =
                    Debug.log "kek2" player
            in
            msgSuccess player

        Err errorMessage ->
            let
                _ =
                    Debug.log "Error in mapIncomingPlayer:" errorMessage
            in
            msgErr
