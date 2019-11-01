module Api exposing (..)

import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import Player exposing (..)


type Msg
    = CreatedRoom (Result Http.Error String)
    | JoinedRoom (Result Http.Error Player)


url =
    -- "http://localhost:4000/api/"
    "http://192.168.0.149:4000/api/"


createRoom =
    Http.post { url = url ++ "create", expect = Http.expectJson CreatedRoom roomDecoder, body = Http.emptyBody }


joinRoom roomId =
    Http.post { url = url ++ "join", expect = Http.expectJson JoinedRoom playerDecoder, body = Http.jsonBody (E.object [ ( "id", E.string roomId ) ]) }


roomDecoder : Decoder String
roomDecoder =
    field "id" string
