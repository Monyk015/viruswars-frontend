module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Flag
import Game exposing (Coord, Game)
import GamePage
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Model exposing (Model, Msg(..), changeRouteTo, subscriptions, update)
import Player exposing (Player)
import Port
import Route exposing (Route(..))
import Url



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init encodedFlag url key =
    let
        maybeFlag =
            Flag.decodeFlag encodedFlag

        ( player, storedRoomId ) =
            case maybeFlag of
                Just flag ->
                    ( Just flag.player, Just flag.roomId )

                Nothing ->
                    ( Nothing, Nothing )

        model =
            { key = key, route = Route.Root, roomId = Nothing, player = player, game = Nothing, storedRoomId = storedRoomId }
    in
    case Route.fromUrl url of
        Just route ->
            changeRouteTo route model

        Nothing ->
            changeRouteTo (Route.Redirect Route.NotFound) model



-- UPDATE
-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.route of
                Home ->
                    Home.viewHome model

                Root ->
                    Home.viewHome model

                NotFound ->
                    div [] [ text "Not Found" ]

                Redirect _ ->
                    div [] [ text "Redirecting" ]

                Room _ ->
                    case model.game of
                        Just game ->
                            GamePage.viewGame game

                        Nothing ->
                            div [] [ text "No game" ]
    in
    { title = "URL Interceptor"
    , body = [ body ]
    }
