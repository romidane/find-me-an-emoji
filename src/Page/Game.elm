module Page.Game exposing (Level(..), Model, Msg(..), init, subscriptions, update, view)

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


type alias Time =
    Int


type Level
    = Level Int Time Time


type alias LevelConfig =
    { gameTime : Int
    , sneakPeakTime : Int
    , numberOfCards : Int
    }


type alias Model =
    { board : List Card
    , time : Time.Posix
    , zone : Time.Zone
    , config : LevelConfig
    , currentLevel : Level
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
    | PileOfPoo


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , config =
            { gameTime = 15
            , sneakPeakTime = 3
            , numberOfCards = 16
            }
      , currentLevel = Level 1 0 0
      }
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
            ( { model | currentLevel = Level 1 0 0 }
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
            ( { model | time = newTime, currentLevel = updateLevelTime model.config model.currentLevel }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


updateLevelTime : LevelConfig -> Level -> Level
updateLevelTime config (Level num gameTime sneakPeakTime) =
    let
        timeElapsed =
            if gameTime < config.gameTime then
                gameTime + 1

            else
                gameTime

        sneakPeakElapsed =
            if sneakPeakTime < config.sneakPeakTime then
                sneakPeakTime + 1

            else
                sneakPeakTime
    in
    Level num timeElapsed sneakPeakElapsed


getTimeElapsed : Level -> ( Int, Int )
getTimeElapsed (Level levelNum elapsedTime peakTimeElapsed) =
    ( elapsedTime, peakTimeElapsed )


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
                    [ div [ class "pure-g" ] (List.map (\card -> viewCard card model) model.board)
                    ]
                ]
            , div [ class "pure-u-1-3" ]
                [ viewSidebar model ]
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

        PileOfPoo ->
            "💩"


viewCard : Card -> Model -> Html Msg
viewCard card model =
    let
        (Level level _ currentSneakPeakTime) =
            model.currentLevel
    in
    section [ class "pure-u-1-4" ]
        [ div
            [ class
                (cssClassNames
                    [ ( "c-card", True )
                    , ( "is-selected", card.selected )
                    , ( "is-fliped", not card.selected && model.config.sneakPeakTime == currentSneakPeakTime )
                    ]
                )
            , id (String.fromInt card.id)
            , onClick (SelectCard card.id)
            ]
            [ div [ class "c-card-inner" ]
                [ div [ class "c-card-front" ]
                    [ span []
                        [ text (viewEmoticon card.emoticon) ]
                    ]
                , div [ class "c-card-back" ]
                    [ span []
                        [ text (viewEmoticon PileOfPoo) ]
                    ]
                ]
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        (Level level _ _) =
            model.currentLevel
    in
    div [ class "c-sidebar" ]
        [ div [ class "pure-g u-mb-charlie" ]
            [ div [ class "pure-u-1-2" ]
                [ h2
                    [ class "c-heading-bravo u-no-margin" ]
                    [ text ("Level: " ++ String.fromInt level) ]
                ]
            , div [ class "pure-u-1-2" ]
                [ button
                    [ class "pure-button pure-button-primary c-btn--pink", onClick NewGame ]
                    [ text "New Game" ]
                ]
            ]
        , viewTimer model
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-2" ] [ text "Emoticon here" ]
            , div [ class "pure-u-1-2" ] [ text "Emoticon here" ]
            ]
        ]


viewTimer : Model -> Html msg
viewTimer model =
    let
        ( timeElapsed, _ ) =
            getTimeElapsed model.currentLevel

        countDown =
            model.config.gameTime - timeElapsed
    in
    div [ class "c-countdown", style "animation-duration" (String.fromInt model.config.gameTime ++ "s") ]
        [ div [ class "c-countdown-number" ]
            [ text (String.fromInt countDown) ]
        , Svg.svg []
            [ Svg.circle
                [ SvgAttr.cx "20", SvgAttr.cy "20", SvgAttr.r "18" ]
                []
            ]
        ]


viewTimer2 : Model -> Html msg
viewTimer2 model =
    div [ class "c-timer-meter u-mb-charlie" ] [ span [ style "width" "100%" ] [] ]



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
