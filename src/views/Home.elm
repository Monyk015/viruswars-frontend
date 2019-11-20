module Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model, Msg(..), changeRouteTo, subscriptions, update)
import Util exposing (..)


viewHome : Model -> Html Msg
viewHome model =
    div []
        [ text "The current room id is: "
        , b [] [ text (roomRouteToRoomId model.route) ]
        , ul []
            [ -- viewLink Route.Home
              li [] [ text (viewRoomLink model.roomId) ]
            , -- , li [] [ a [ href "#/some-shit-link" ] [ text "kek" ] ]
              button
                [ onClick CreateRoom ]
                [ text "create room" ]
            , button [ onClick RedirectToCreatedRoom ] [ text "join room" ]
            ]
        , viewPlayer model.player
        ]
