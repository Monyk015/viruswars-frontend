module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

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


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , roomId : Maybe String
    , player : Maybe Player
    , game : Maybe Game
    , storedRoomId : Maybe String
    }


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
                    let
                        _ =
                            Debug.log "err" error
                    in
                    ( model, Cmd.none )

                Api.JoinedRoom (Ok newPlayer) ->
                    case model.roomId of
                        Just roomId ->
                            ( { model | player = Just newPlayer }, Port.joinRoom (E.object [ ( "playerId", E.string newPlayer.id ), ( "player", E.string (Player.viewPlayerType newPlayer) ), ( "roomId", E.string roomId ) ]) )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "something went terribly wrong here" "joining room with no room id"
                            in
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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.route of
                Home ->
                    viewHome model

                Root ->
                    viewHome model

                NotFound ->
                    div [] [ text "Not Found" ]

                Redirect _ ->
                    div [] [ text "Redirecting" ]

                Room _ ->
                    case model.game of
                        Just game ->
                            Game.viewGame game Move

                        Nothing ->
                            div [] [ text "No game" ]
    in
    { title = "URL Interceptor"
    , body = [ body ]
    }


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


projectUrl =
    "localhost:3000/#"


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


viewLink : Route -> Html msg
viewLink route =
    li [] [ a [ href (Route.routeToString route) ] [ text (Route.routeToString route) ] ]
