module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page.About as About
import Page.Error404 as Error404
import Page.Game as Game
import Task
import Time
import Url
import Url.Parser exposing (Parser, int, map, oneOf, parse, s, top)



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- ROUTES


type Route
    = About
    | Game
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map About (s "about")
        , map Game top
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (parse route url)


routeFromUrl : Url.Url -> Route
routeFromUrl =
    toRoute << Url.toString



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Route
    , about : About.Model
    , game : Game.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
    let
        ( gameModel, gameMsg ) =
            Game.init ()

        ( aboutModel, aboutMsg ) =
            About.init ()
    in
    ( { key = key
      , url = routeFromUrl url
      , about = aboutModel
      , game = gameModel
      }
    , Cmd.batch
        [ Cmd.map GameMsg gameMsg
        , Cmd.map AboutMsg aboutMsg
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AboutMsg About.Msg
    | GameMsg Game.Msg


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
            ( { model | url = routeFromUrl url }
            , Cmd.none
            )

        AboutMsg _ ->
            ( model, Cmd.none )

        GameMsg newMsg ->
            let
                ( gameModel, cmd ) =
                    Game.update newMsg model.game
            in
            ( { model | game = gameModel }
            , Cmd.map (\a -> GameMsg a) cmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameMsg (Game.subscriptions model.game) ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Find me"
    , body =
        [ div [ class "o-main" ]
            [ viewNavigation model
            , viewPage model
            ]
        ]
    }


viewNavigation : Model -> Html msg
viewNavigation model =
    div [ class "c-page-menu pure-menu pure-menu-horizontal" ]
        [ div [ class "o-container" ]
            [ a [ class "pure-menu-heading", href "/" ]
                [ text "Find me" ]
            , ul [ class "pure-menu-list" ]
                [ viewLink "/" "Home" (model.url == Game)
                , viewLink "/about" "About" (model.url == About)
                ]
            ]
        ]


viewLink : String -> String -> Bool -> Html msg
viewLink path name bool =
    let
        cssClass =
            if bool then
                "pure-menu-item pure-menu-selected"

            else
                "pure-menu-item"
    in
    li [ class cssClass ]
        [ a [ class "pure-menu-link", href path ]
            [ text name ]
        ]


viewPage : Model -> Html Msg
viewPage model =
    div [ class "o-container" ]
        [ case model.url of
            About ->
                Html.map AboutMsg (About.view model.about)

            Game ->
                Html.map GameMsg (Game.view model.game)

            NotFound ->
                Error404.view
        ]
