module Page.Game exposing (Card, Model, Msg(..), update, view)

import Array exposing (Array, fromList, get, length)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Random
import String



-- MODEL


type Emoticon
    = GRINNING_FACE
    | TEARS_OF_JOY_FACE
    | SMIRKING_FACE


type alias Model =
    { board : List Card
    }


type alias Card =
    { id : Int
    , selected : Bool
    , emoticon : Emoticon
    }



-- UPDATE


type Msg
    = NewGame
    | NewEmoticon Emoticon
    | SelectCard Int


defaultList =
    [ Card 1 False GRINNING_FACE, Card 2 False TEARS_OF_JOY_FACE, Card 3 False SMIRKING_FACE ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | board = defaultList }, Cmd.none )

        NewEmoticon e ->
            ( model, Cmd.none )

        SelectCard id ->
            ( { model | board = model.board }, Cmd.none )



-- VIEW


viewEmoticon : Emoticon -> String
viewEmoticon emoticon =
    case emoticon of
        GRINNING_FACE ->
            "😁"

        TEARS_OF_JOY_FACE ->
            "😂"

        SMIRKING_FACE ->
            "😏"


view : Model -> (Msg -> msg) -> Html msg
view model appMsg =
    div []
        [ h1 [] [ text "Let's play" ]
        , div [ class "grid" ] (List.map (\card -> viewCell card appMsg) model.board)
        ]


viewCell : Card -> (Msg -> msg) -> Html msg
viewCell card appMsg =
    div [ class "pure-u-1-4" ]
        [ div [ class "c-card", id (String.fromInt card.id), onClick (appMsg (SelectCard card.id)) ]
            [ span [] [ text (viewEmoticon card.emoticon) ] ]
        ]
