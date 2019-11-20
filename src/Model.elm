module Model exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Flag
import Game exposing (Coord, Game)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Player exposing (Player)
import Port
import Route exposing (Route(..))
import Url


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , roomId : Maybe String
    , player : Maybe Player
    , game : Maybe Game
    , storedRoomId : Maybe String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ApiMsg Api.Msg
    | CreateRoom
    | RedirectToCreatedRoom
    | GotGame Game
    | Move Coord
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Route.fromUrl url of
                Just route ->
                    changeRouteTo route model

                Nothing ->
                    changeRouteTo (Route.Redirect Route.NotFound) model

        ApiMsg apiMsg ->
            case apiMsg of
                Api.CreatedRoom (Ok newRoomId) ->
                    ( { model | roomId = Just newRoomId }, Cmd.none )

                -- changeRouteTo (Route.Redirect (Route.Room newRoomId)) model
                Api.CreatedRoom (Err error) ->
                    ( model, Cmd.none )

                Api.JoinedRoom (Ok newPlayer) ->
                    case model.roomId of
                        Just roomId ->
                            ( { model | player = Just newPlayer }, Port.joinRoom (E.object [ ( "playerId", E.string newPlayer.id ), ( "player", E.string (Player.viewPlayerType newPlayer) ), ( "roomId", E.string roomId ) ]) )

                        Nothing ->
                            ( model, Cmd.none )

                Api.JoinedRoom (Err error) ->
                    ( model, Cmd.none )

        CreateRoom ->
            ( model, Cmd.map ApiMsg Api.createRoom )

        RedirectToCreatedRoom ->
            case model.roomId of
                Just roomId ->
                    changeRouteTo (Route.Redirect (Route.Room roomId)) model

                Nothing ->
                    ( model, Cmd.none )

        GotGame newGame ->
            ( { model | game = Just newGame }, Cmd.none )

        Move ( i, j ) ->
            ( model, Port.move (E.list E.int [ i, j ]) )

        NoOp ->
            ( model, Cmd.none )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Redirect redirectTo ->
            ( { model | route = route }, changeUrl redirectTo model )

        NotFound ->
            ( { model | route = route }, Cmd.none )

        Room id ->
            let
                cmd =
                    case ( model.player, model.storedRoomId ) of
                        ( Just player, Just storedRoomId ) ->
                            if id == storedRoomId then
                                Port.joinRoom (E.object [ ( "playerId", E.string player.id ), ( "player", E.string (Player.viewPlayerType player) ), ( "roomId", E.string id ) ])

                            else
                                Cmd.map ApiMsg (Api.joinRoom id)

                        _ ->
                            Cmd.map ApiMsg (Api.joinRoom id)
            in
            ( { model | route = route, roomId = Just id }, cmd )

        Root ->
            ( { model | route = route }, Cmd.none )

        Home ->
            ( { model | route = route }, Cmd.none )


changeUrl : Route -> Model -> Cmd Msg
changeUrl route model =
    Nav.pushUrl model.key (Route.routeToString route)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.gotGame (Game.mapIncomingGame GotGame NoOp)
