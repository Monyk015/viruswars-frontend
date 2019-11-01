port module Port exposing (..)

import Json.Encode as E


port joinRoom : E.Value -> Cmd msg


port gotGame : (E.Value -> msg) -> Sub msg


port move : E.Value -> Cmd msg
