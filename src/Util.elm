module Util exposing (..)

import Env exposing (projectUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Player exposing (Player)
import Route exposing (Route(..))


viewLink : Route -> Html msg
viewLink route =
    li [] [ a [ href (Route.routeToString route) ] [ text (Route.routeToString route) ] ]


viewRoomLink : Maybe String -> String
viewRoomLink maybeRoomId =
    case maybeRoomId of
        Just roomId ->
            projectUrl ++ "room/" ++ roomId

        Nothing ->
            "No room created"


viewPlayer : Maybe Player -> Html Msg
viewPlayer maybePlayer =
    case maybePlayer of
        Nothing ->
            text "No player"

        Just player ->
            div []
                [ div []
                    [ text player.id ]
                , div
                    []
                    [ text (Player.viewPlayerType player) ]
                ]


roomRouteToRoomId : Route -> String
roomRouteToRoomId route =
    case route of
        Room id ->
            id

        _ ->
            ""
