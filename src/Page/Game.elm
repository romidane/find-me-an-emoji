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


type alias Model =
    { board : List Card
    }


type alias Id =
    Int


type alias Card =
    { id : Id
    , selected : Bool
    , emoticon : Emoticon
    }


type Emoticon
    = GrinningFace
    | TearsOfJoyFace
    | SmirkingFace
    | LookOfTriumphFace
    | RelievedFace
    | KissingFace
    | HeartShapedEyesFace
    | UnamusedFace
    | SleepyFace
    | FearfulFace
    | StuckOutTongueClosedEyesFace
    | DizzyFace



-- UPDATE


type Msg
    = NewGame
    | NewCards (List Emoticon)
    | SelectCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Random.generate NewCards (Random.list 12 cardGenerator) )

        SelectCard id ->
            ( { model | board = handleSelection id model.board }, Cmd.none )

        NewCards list ->
            ( { model | board = generateCardList list }, Cmd.none )


generateCardList : List Emoticon -> List Card
generateCardList list =
    List.indexedMap (\index emoticon -> Card index False emoticon) list


cardGenerator : Random.Generator Emoticon
cardGenerator =
    Random.uniform GrinningFace
        [ TearsOfJoyFace
        , SmirkingFace
        , LookOfTriumphFace
        , RelievedFace
        , KissingFace
        , HeartShapedEyesFace
        , UnamusedFace
        , SleepyFace
        , FearfulFace
        , StuckOutTongueClosedEyesFace
        , DizzyFace
        ]


handleSelection : Id -> List Card -> List Card
handleSelection id =
    List.map
        (\card ->
            if card.id == id then
                { card | selected = not card.selected }

            else
                card
        )



-- VIEW


view : Model -> (Msg -> msg) -> Html msg
view model appMsg =
    div []
        [ h1 [] [ text "Let's play" ]
        , div [ class "grid" ] (List.map (\card -> viewCard card appMsg) model.board)
        , button [ onClick (appMsg NewGame) ] [ text "Click" ]
        ]


viewEmoticon : Emoticon -> String
viewEmoticon emoticon =
    case emoticon of
        GrinningFace ->
            "😁"

        TearsOfJoyFace ->
            "😂"

        SmirkingFace ->
            "😏"

        LookOfTriumphFace ->
            "😤"

        RelievedFace ->
            "😌"

        KissingFace ->
            "😚"

        HeartShapedEyesFace ->
            "😍"

        UnamusedFace ->
            "😒"

        SleepyFace ->
            "😪"

        FearfulFace ->
            "😨"

        StuckOutTongueClosedEyesFace ->
            "😝"

        DizzyFace ->
            "😵"


viewCard : Card -> (Msg -> msg) -> Html msg
viewCard card appMsg =
    section [ class "pure-u-1-4" ]
        [ div
            [ class
                (cssClassNames
                    [ ( "c-card", True )
                    , ( "is-selected", card.selected )
                    ]
                )
            , id (String.fromInt card.id)
            , onClick (appMsg (SelectCard card.id))
            ]
            [ span [] [ text (viewEmoticon card.emoticon) ] ]
        ]



-- Utilities


cssClassNames : List ( String, Bool ) -> String
cssClassNames list =
    List.foldl
        (\( key, value ) acc ->
            if value == True then
                acc ++ " " ++ key

            else
                acc
        )
        ""
        list
        |> String.trimLeft
