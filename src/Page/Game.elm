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


type alias CurrentTargets =
    ( Emoticon, Emoticon )


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
    , currentTargets : CurrentTargets
    , gameStarted : Bool
    }


type alias CardId =
    Int


type alias Card =
    { id : CardId
    , selected : Bool
    , emoticon : Emoticon
    , revealTime : Int
    , matched : Bool
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
    | SeeNoEvilMonkey
    | NoFace


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , config =
            { gameTime = 30
            , sneakPeakTime = 3
            , numberOfCards = 20
            }
      , currentLevel = Level 1 0 0
      , currentTargets = ( NoFace, NoFace )
      , gameStarted = False
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
    | PairOfCards CurrentTargets
    | SelectCard Int
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | UpdateLevelCompletion
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | currentLevel = Level 1 0 0 }
            , Cmd.batch
                [ Random.generate PairOfCards (Random.pair cardGenerator cardGenerator)
                ]
            )

        StartGame ->
            ( { model | gameStarted = True }, Cmd.none )

        SelectCard id ->
            ( { model | board = updateCardSelection id model.currentTargets model.board }
            , Task.succeed UpdateLevelCompletion |> Task.perform identity
            )

        NewCards list ->
            ( { model | board = generateCardList list }
            , Cmd.none
            )

        Tick newTime ->
            ( if model.gameStarted then
                { model
                    | time = newTime
                    , currentLevel = updateLevelTime model.config model.currentLevel
                    , board = updateCardsTime model.config model.currentTargets model.board
                }

              else
                model
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        PairOfCards currentTargets ->
            ( { model | currentTargets = currentTargets }
            , Random.generate NewCards <|
                Random.list model.config.numberOfCards <|
                    weightedCardGenerator currentTargets
            )

        UpdateLevelCompletion ->
            let
                ( target1, target2 ) =
                    model.currentTargets

                target1InBoard =
                    model.board
                        |> List.filter (\card -> card.emoticon == target1)

                target2InBoard =
                    model.board
                        |> List.filter (\card -> card.emoticon == target2)

                allTarget1Complete =
                    target1InBoard |> List.all (\card -> card.matched)
            in
            ( model, Cmd.none )


updateLevelTime : LevelConfig -> Level -> Level
updateLevelTime config (Level num gameTime sneakPeakTime) =
    let
        timeElapsed =
            if gameTime < config.gameTime && sneakPeakTime >= config.sneakPeakTime then
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


updateCardsTime : LevelConfig -> CurrentTargets -> List Card -> List Card
updateCardsTime config targets =
    List.map
        (\card ->
            let
                isATarget =
                    cardIsATarget targets card

                timeExpired =
                    card.revealTime > config.sneakPeakTime
            in
            if card.selected && not isATarget && not timeExpired then
                { card | revealTime = card.revealTime + 1 }

            else if card.selected && timeExpired then
                { card | revealTime = 0, selected = False }

            else
                card
        )


updateCardSelection : CardId -> CurrentTargets -> List Card -> List Card
updateCardSelection id targets =
    List.map
        (\card ->
            if card.id == id then
                if cardIsATarget targets card then
                    { card | selected = True, revealTime = 0 }

                else
                    { card | selected = not card.selected }

            else
                card
        )


cardIsATarget : CurrentTargets -> Card -> Bool
cardIsATarget ( target1, target2 ) card =
    List.member card.emoticon [ target1, target2 ]


generateCardList : List Emoticon -> List Card
generateCardList list =
    List.indexedMap (\index emoticon -> Card index False emoticon 2 False) list


listOfEmoticons : List Emoticon
listOfEmoticons =
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
    , GrinningFace
    ]


cardGenerator : Random.Generator Emoticon
cardGenerator =
    Random.uniform GrinningFace listOfEmoticons


weightedCardGenerator : CurrentTargets -> Random.Generator Emoticon
weightedCardGenerator ( emoticon1, emoticon2 ) =
    let
        list =
            listOfEmoticons
                |> List.filter (\emoticon -> not (emoticon == emoticon1))
                |> List.map
                    (\emoticon ->
                        if emoticon == emoticon2 then
                            ( 40, emoticon )

                        else
                            ( 10, emoticon )
                    )
    in
    Random.weighted ( 40, emoticon1 ) list



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
                    [ viewStart model
                    , div [ class "pure-g" ] (List.map (\card -> section [ class "pure-u-1-5" ] [ viewCard card model ]) model.board)
                    , viewGameOver model
                    ]
                ]
            , div [ class "pure-u-1-3" ]
                [ viewSidebar model ]
            ]
        ]


viewStart : Model -> Html Msg
viewStart model =
    div [ class (cssClassNames [ ( "c-game-start", True ), ( "u-hidden", model.gameStarted ) ]) ]
        [ div [ class "c-game-start__content" ]
            [ h2 [ class "c-heading-bravo u-no-margin" ] [ text "Welcome challenger!" ]
            , div [ class "c-game-start__instructions" ] [ viewEmojiTargets model ]
            , button [ class "pure-button pure-button-primary c-btn--pink", onClick StartGame ] [ text "I'm ready" ]
            ]
        ]


viewGameOver : Model -> Html Msg
viewGameOver model =
    let
        (Level _ timeElapsed _) =
            model.currentLevel

        timesUp =
            timeElapsed == model.config.gameTime
    in
    div
        [ class (cssClassNames [ ( "c-game-over", True ), ( "u-hidden", not timesUp ) ]) ]
        [ div [ class "c-game-over__content" ]
            [ h2 [ class "u-no-margin-top" ]
                [ text "Game Over" ]
            , button
                [ class "pure-button pure-button-primary c-btn--pink", onClick NewGame ]
                [ text "Play a new game" ]
            ]
        ]


viewEmoticon : Emoticon -> String
viewEmoticon emoticon =
    case emoticon of
        NoFace ->
            ""

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

        SeeNoEvilMonkey ->
            "🙈"


viewCard : Card -> Model -> Html Msg
viewCard card model =
    let
        (Level level _ currentSneakPeakTime) =
            model.currentLevel
    in
    div
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
                    [ text (viewEmoticon SeeNoEvilMonkey) ]
                ]
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        (Level level _ _) =
            model.currentLevel
    in
    aside [ class "c-sidebar" ]
        [ div [ class "pure-g u-mb-bravo" ]
            [ div [ class "pure-u-1-2" ]
                [ h2
                    [ class "c-heading-bravo u-no-margin" ]
                    [ text ("Level: " ++ String.fromInt level) ]
                ]
            , div [ class "pure-u-1-2" ]
                [ button
                    [ class "pure-button pure-button-primary c-btn--pink u-float-right", onClick NewGame ]
                    [ text "New Game" ]
                ]
            ]
        , viewTimer model
        , if model.gameStarted then
            div [ class "c-sidebar__instruction-container" ] [ viewEmojiTargets model ]

          else
            text ""
        ]


viewTimer : Model -> Html msg
viewTimer model =
    let
        (Level _ timeElapsed _) =
            model.currentLevel

        countDown =
            (toFloat timeElapsed / toFloat model.config.gameTime) * 100
    in
    div [ class "c-timer-meter" ] [ span [ style "width" (String.fromFloat countDown ++ "%") ] [] ]


viewEmojiTargets : Model -> Html msg
viewEmojiTargets model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ]
            [ p [ class "c-text" ] [ text "Find all of the following emoticons before the time runs out!" ] ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ] [ text (viewEmoticon (Tuple.first model.currentTargets)) ] ]
        , div [ class "pure-u-1-2" ] [ span [ class "c-icon__emoticon" ] [ text (viewEmoticon (Tuple.second model.currentTargets)) ] ]
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
