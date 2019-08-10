module Page.Game exposing (Card, Model, Msg(..), init, subscriptions, update, view)

import Array exposing (Array, fromList, get, length)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Random
import String
import Svg
import Svg.Attributes as SvgAttr
import Task
import Time



-- MODEL


type alias Model =
    { board : List Card
    , currentLevel : Int
    , time : Time.Posix
    , zone : Time.Zone
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = [], currentLevel = 1, time = Time.millisToPosix 0, zone = Time.utc }
    , Cmd.batch
        [ Task.succeed NewGame |> Task.perform identity
        , Task.perform AdjustTimeZone Time.here
        ]
    )



-- UPDATE


type Msg
    = NewGame
    | NewCards (List Emoticon)
    | SelectCard Int
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model
            , Random.generate NewCards (Random.list 16 cardGenerator)
            )

        SelectCard id ->
            ( { model | board = handleSelection id model.board }
            , Cmd.none
            )

        NewCards list ->
            ( { model | board = generateCardList list }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "o-page" ]
        [ div [ class "pure-g" ]
            [ div [ class "pure-u-2-3" ]
                [ div [ class "c-game-container" ]
                    [ div [ class "pure-g" ] (List.map (\card -> viewCard card) model.board)
                    ]
                ]
            , div [ class "pure-u-1-3" ]
                [ viewSidebar model ]
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "c-header" ]
        [ div [ class "pure-g" ]
            [ div [ class "pure-u-1-2" ]
                [ span [] [ text "....." ] ]
            , div [ class "pure-u-1-2 u-ta-right" ]
                [ button [ class "pure-button pure-button-primary c-btn--pink", onClick NewGame ] [ text "Start a new came" ] ]
            ]
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


viewCard : Card -> Html Msg
viewCard card =
    section [ class "pure-u-1-4" ]
        [ div
            [ class
                (cssClassNames
                    [ ( "c-card", True )
                    , ( "is-selected", card.selected )
                    ]
                )
            , id (String.fromInt card.id)
            , onClick (SelectCard card.id)
            ]
            [ span [] [ text (viewEmoticon card.emoticon) ] ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "c-sidebar" ]
        [ div [ class "pure-g u-mb-charlie" ]
            [ div [ class "pure-u-1-2" ]
                [ h2
                    [ class "c-heading-bravo u-no-margin" ]
                    [ text ("Level: " ++ String.fromInt model.currentLevel) ]
                ]
            , div [ class "pure-u-1-2" ]
                [ button
                    [ class "pure-button pure-button-primary c-btn--pink", onClick NewGame ]
                    [ text "New Game" ]
                ]
            ]
        , div [ class "c-timer-meter u-mb-charlie" ] [ span [] [ text "" ] ]
        , div [ class "c-countdown" ]
            [ div [ class "c-countdown-number" ]
                [ text "1" ]
            , Svg.svg []
                [ Svg.circle
                    [ SvgAttr.cx "20", SvgAttr.cy "20", SvgAttr.r "18" ]
                    []
                ]
            ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-2" ] [ text "Emoticon here" ]
            , div [ class "pure-u-1-2" ] [ text "Emoticon here" ]
            ]
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
