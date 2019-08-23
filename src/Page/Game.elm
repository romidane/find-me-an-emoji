module Page.Game exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Random
import Random.List exposing (shuffle)
import String
import Svg
import Svg.Attributes as SvgAttr
import Task
import Time



-- MODEL


type alias CurrentTargets =
    ( Emoticon, Emoticon )


type alias GameBoard =
    List (Card CardConfig)


type alias CardId =
    Int


type alias CssClassList =
    List ( String, Bool )


type alias CardConfig =
    { id : CardId
    , emoticon : Emoticon
    , revealTime : Int
    }


type alias Level =
    { level : Int
    , timeElapsed : Int
    , sneakPeakTimeElapsed : Int
    , incorrectSelections : Int
    }


type alias LevelConfig =
    { gameTime : Int
    , sneakPeakTime : Int
    , numberOfCards : Int
    , currentLevel : Level
    , numberOfTargets : Int
    }


type alias Model =
    { board : GameBoard
    , time : Time.Posix
    , zone : Time.Zone
    , config : LevelConfig
    , currentTargets : CurrentTargets
    , gameStatus : GameStatus
    }


type GameStatus
    = GameStarted
    | GameIdle
    | GameOver


type Card config
    = RevealedCard config
    | SelectedCard config
    | HiddenCard config
    | MatchedCard config
    | ShakingCard config


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


defaultGameConfig : LevelConfig
defaultGameConfig =
    { currentLevel =
        { level = 1
        , timeElapsed = 0
        , sneakPeakTimeElapsed = 0
        , incorrectSelections = 0
        }
    , gameTime = 45
    , sneakPeakTime = 4
    , numberOfCards = 20
    , numberOfTargets = 3
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , config = defaultGameConfig
      , currentTargets = ( NoFace, NoFace )
      , gameStatus = GameIdle
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
    | UpdateGameStatus
    | StartGame
    | NextLevel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | config = defaultGameConfig, gameStatus = GameIdle }
            , Cmd.batch
                [ generateRandomPair
                ]
            )

        NextLevel ->
            ( { model | config = updateLevel model.config, gameStatus = GameIdle }
            , Cmd.batch
                [ generateRandomPair
                ]
            )

        StartGame ->
            ( { model | gameStatus = GameStarted }, Cmd.none )

        SelectCard id ->
            ( { model | board = updateCardSelection id model.currentTargets model.board }
            , Task.succeed UpdateLevelCompletion |> Task.perform identity
            )

        NewCards list ->
            ( { model | board = generateCardList model.config list }
            , Cmd.none
            )

        Tick newTime ->
            case model.gameStatus of
                GameStarted ->
                    ( { model
                        | time = newTime
                        , config = updateLevelTime model.config
                        , board = updateCardsTime model.config model.currentTargets model.board
                      }
                    , Task.succeed UpdateGameStatus |> Task.perform identity
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        PairOfCards currentTargets ->
            generateNewCards model currentTargets

        UpdateLevelCompletion ->
            updateLevelCompletion model

        UpdateGameStatus ->
            ( { model | gameStatus = updateGameStatus model }
            , Cmd.none
            )


updateLevel : LevelConfig -> LevelConfig
updateLevel config =
    let
        currLevel =
            config.currentLevel

        nextLevel =
            currLevel.level + 1
    in
    if modBy nextLevel 5 == 0 then
        { config
            | currentLevel = { currLevel | level = nextLevel }
            , gameTime = config.gameTime - 5
            , numberOfCards = config.numberOfCards + 5
            , numberOfTargets =
                if config.numberOfTargets > 1 then
                    config.numberOfTargets - 1

                else
                    config.numberOfTargets
        }

    else
        { config | currentLevel = Level nextLevel 0 0 0 }


updateGameStatus : Model -> GameStatus
updateGameStatus model =
    let
        levelConfig =
            model.config.currentLevel
    in
    if levelConfig.timeElapsed >= model.config.gameTime then
        GameOver

    else
        model.gameStatus


updateLevelCompletion : Model -> ( Model, Cmd Msg )
updateLevelCompletion model =
    let
        ( target1, target2 ) =
            model.currentTargets

        allTargetsComplete =
            listOfEmoticonsOf target1 model.board
                |> List.append (listOfEmoticonsOf target2 model.board)
                |> List.all
                    (\card ->
                        case card of
                            MatchedCard _ ->
                                True

                            _ ->
                                False
                    )
    in
    if allTargetsComplete then
        ( model, Task.succeed NextLevel |> Task.perform identity )

    else
        ( model, Cmd.none )


updateGameConfig : LevelConfig -> LevelConfig
updateGameConfig config =
    if config.gameTime > 25 && config.numberOfCards < 30 then
        { config
            | gameTime = config.gameTime - 3
            , numberOfCards = config.numberOfCards + 5
        }

    else
        config


updateLevelTime : LevelConfig -> LevelConfig
updateLevelTime config =
    let
        { level, timeElapsed, sneakPeakTimeElapsed } =
            config.currentLevel

        time =
            if timeElapsed < config.gameTime && sneakPeakTimeElapsed >= config.sneakPeakTime then
                timeElapsed + 1

            else
                timeElapsed

        sneakPeakTime =
            if sneakPeakTimeElapsed < config.sneakPeakTime then
                sneakPeakTimeElapsed + 1

            else
                sneakPeakTimeElapsed
    in
    { config | currentLevel = Level level time sneakPeakTime 0 }


updateCardsTime : LevelConfig -> CurrentTargets -> GameBoard -> GameBoard
updateCardsTime config targets =
    List.map
        (\card ->
            let
                conf =
                    getCardConfig card

                timeExpired =
                    conf.revealTime >= config.sneakPeakTime

                resetRevealTime =
                    { conf | revealTime = 0 }

                updatedRevealTime =
                    { conf | revealTime = conf.revealTime + 1 }
            in
            case card of
                RevealedCard cardConfig ->
                    if timeExpired then
                        HiddenCard resetRevealTime

                    else
                        RevealedCard updatedRevealTime

                SelectedCard cardConfig ->
                    if timeExpired then
                        HiddenCard resetRevealTime

                    else
                        SelectedCard updatedRevealTime

                _ ->
                    card
        )


updateCardSelection : CardId -> CurrentTargets -> GameBoard -> GameBoard
updateCardSelection id targets =
    List.map
        (\card ->
            let
                config =
                    getCardConfig card
            in
            if config.id == id then
                case card of
                    RevealedCard _ ->
                        card

                    HiddenCard _ ->
                        if cardIsATarget targets card then
                            MatchedCard { config | revealTime = 0 }

                        else
                            SelectedCard config

                    _ ->
                        card

            else
                card
        )


cardIsATarget : CurrentTargets -> Card CardConfig -> Bool
cardIsATarget ( target1, target2 ) card =
    let
        config =
            getCardConfig card
    in
    List.member config.emoticon [ target1, target2 ]


generateCardList : LevelConfig -> List Emoticon -> GameBoard
generateCardList config =
    List.indexedMap
        (\index emoticon ->
            RevealedCard
                { id = index
                , emoticon = emoticon
                , revealTime = 0
                }
        )


listOfEmoticonsOf : Emoticon -> GameBoard -> GameBoard
listOfEmoticonsOf target board =
    board
        |> List.filter
            (\card ->
                let
                    config =
                        getCardConfig card
                in
                config.emoticon == target
            )


generateRandomPair : Cmd Msg
generateRandomPair =
    Random.generate PairOfCards <| Random.pair cardGenerator cardGenerator


getCardConfig : Card CardConfig -> CardConfig
getCardConfig card =
    case card of
        RevealedCard config ->
            config

        SelectedCard config ->
            config

        HiddenCard config ->
            config

        MatchedCard config ->
            config

        ShakingCard config ->
            config


generateNewCards : Model -> CurrentTargets -> ( Model, Cmd Msg )
generateNewCards model ( emoticon1, emoticon2 ) =
    if emoticon1 == emoticon2 then
        ( model, generateRandomPair )

    else
        ( { model | currentTargets = ( emoticon1, emoticon2 ) }
        , Random.generate NewCards
            (Random.list (model.config.numberOfCards - (model.config.numberOfTargets * 2)) (weightedCardGenerator ( emoticon1, emoticon2 ))
                |> Random.andThen
                    (\weightedList ->
                        let
                            newList =
                                weightedList
                                    |> List.append (List.repeat model.config.numberOfTargets emoticon1)
                                    |> List.append (List.repeat model.config.numberOfTargets emoticon2)
                        in
                        shuffle newList
                    )
            )
        )


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
                |> List.filter (\emoticon -> not (emoticon == emoticon2))
                |> List.map
                    (\emoticon ->
                        ( 20, emoticon )
                    )
    in
    Random.weighted ( 0, emoticon1 ) list



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
                    , div [ class "pure-g" ]
                        (model.board
                            |> List.map
                                (\card ->
                                    section [ class "pure-u-1-5" ]
                                        [ viewCard card ]
                                )
                        )
                    , viewGameOver model.gameStatus
                    ]
                ]
            , div [ class "pure-u-1-3" ]
                [ viewSidebar model ]
            ]
        ]


viewStart : Model -> Html Msg
viewStart model =
    let
        { level } =
            model.config.currentLevel
    in
    case model.gameStatus of
        GameIdle ->
            div [ class (cssClassNames [ ( "c-game-start", True ) ]) ]
                [ div [ class "c-game-start__content" ]
                    [ h2 [ class "c-heading-bravo u-no-margin" ] [ text <| "Level: " ++ String.fromInt level ]
                    , div [ class "c-game-start__instructions" ] [ viewEmojiTargets model ]
                    , button [ class "pure-button pure-button-primary c-btn--pink", onClick StartGame ] [ text "Let's go!" ]
                    ]
                ]

        _ ->
            text ""


viewGameOver : GameStatus -> Html Msg
viewGameOver status =
    case status of
        GameOver ->
            div
                [ class (cssClassNames [ ( "c-game-over", True ) ]) ]
                [ div [ class "c-game-over__content" ]
                    [ h2 [ class "u-no-margin-top" ]
                        [ text "Game Over" ]
                    , button
                        [ class "pure-button pure-button-primary c-btn--pink", onClick NewGame ]
                        [ text "Play a new game" ]
                    ]
                ]

        _ ->
            text ""


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


viewCard : Card CardConfig -> Html Msg
viewCard card =
    case card of
        SelectedCard config ->
            viewCardItem [ ( "is-selected", True ) ] config

        HiddenCard config ->
            viewCardItem [ ( "is-fliped", True ) ] config

        RevealedCard config ->
            viewCardItem [] config

        MatchedCard config ->
            viewCardItem [ ( "is-matched", True ) ] config

        ShakingCard config ->
            viewCardItem [ ( "is-fliped", True ), ( "is-shaking", True ) ] config


viewCardItem : CssClassList -> CardConfig -> Html Msg
viewCardItem css config =
    div
        [ class
            (cssClassNames (List.append [ ( "c-card", True ) ] css))
        , id (String.fromInt config.id)
        , onClick (SelectCard config.id)
        ]
        [ div [ class "c-card-inner" ]
            [ div [ class "c-card-front" ]
                [ span []
                    [ text (viewEmoticon config.emoticon) ]
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
        { level } =
            model.config.currentLevel
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
        , case model.gameStatus of
            GameStarted ->
                div [ class "c-sidebar__instruction-container" ]
                    [ viewEmojiTargets model ]

            _ ->
                text ""
        ]


viewTimer : Model -> Html msg
viewTimer model =
    let
        { timeElapsed } =
            model.config.currentLevel

        countDown =
            (toFloat timeElapsed / toFloat model.config.gameTime) * 100
    in
    div [ class "c-timer-meter" ] [ span [ style "width" (String.fromFloat countDown ++ "%") ] [] ]


viewEmojiTargets : Model -> Html msg
viewEmojiTargets model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ]
            [ p [ class "c-text" ]
                [ text "Find all of the following emoticons before the time runs out!" ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (viewEmoticon (Tuple.first model.currentTargets)) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets (Tuple.first model.currentTargets) model.board) ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (viewEmoticon (Tuple.second model.currentTargets)) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets (Tuple.second model.currentTargets) model.board) ]
            ]
        ]



-- Utilities


countNumberOfTargets : Emoticon -> GameBoard -> Int
countNumberOfTargets target board =
    board
        |> List.filter
            (\card ->
                let
                    config =
                        getCardConfig card
                in
                config.emoticon == target
            )
        |> List.foldl
            (\card acc ->
                case card of
                    MatchedCard _ ->
                        acc

                    _ ->
                        acc + 1
            )
            0


cssClassNames : CssClassList -> String
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
