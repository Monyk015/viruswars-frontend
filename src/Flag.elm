module Flag exposing (..)

import Json.Decode as D exposing (..)
import Player exposing (Player)


type alias Flag =
    { player : Player
    , roomId : String
    }


flagDecoder : Decoder Flag
flagDecoder =
    map2 Flag (field "player" Player.playerDecoder) (field "roomId" string)


decodeFlag : D.Value -> Maybe Flag
decodeFlag val =
    case D.decodeValue flagDecoder val of
        Ok flag ->
            Just flag

        Err errorMessage ->
            Nothing
