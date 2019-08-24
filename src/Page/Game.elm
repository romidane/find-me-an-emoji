module Page.Game exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Data.Game as Data
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
    ( Data.Emoticon, Data.Emoticon )


type alias GameBoard =
    List (Card CardConfig)


type alias CardId =
    Int


type alias CssClassList =
    List ( String, Bool )


type alias CardConfig =
    { id : CardId
    , emoticon : Data.Emoticon
    , revealTime : Int
    }


type alias Level =
    { level : Int
    , timeElapsed : Int
    , sneakPeakTimeElapsed : Int
    , incorrectSelections : Int
    , shakeTimeElapsed : Int
    }


type alias LevelConfig =
    { gameTime : Int
    , errorTreshold : Int
    , sneakPeakTime : Int
    , numberOfCards : Int
    , currentLevel : Level
    , numberOfTargets : Int
    , currentTargets : CurrentTargets
    }


type alias Model =
    { board : GameBoard
    , time : Time.Posix
    , zone : Time.Zone
    , config : LevelConfig
    , gameStatus : GameStatus
    }


type GameStatus
    = GameStarted
    | GameIdle
    | GameOver
    | GamePaused


type Card config
    = RevealedCard config
    | SelectedCard config
    | HiddenCard config
    | MatchedCard config
    | ShakingCard config


defaultGameConfig : LevelConfig
defaultGameConfig =
    { currentLevel =
        { level = 1
        , timeElapsed = 0
        , sneakPeakTimeElapsed = 0
        , incorrectSelections = 0
        , shakeTimeElapsed = 0
        }
    , errorTreshold = 5
    , gameTime = 45
    , sneakPeakTime = 4
    , numberOfCards = 25
    , numberOfTargets = 3
    , currentTargets = ( Data.NoFace, Data.NoFace )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , config = defaultGameConfig
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
    | NewCards (List Data.Emoticon)
    | PairOfCards CurrentTargets
    | SelectCard CardId
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | UpdateLevelCompletion
    | UpdateGameStatus
    | StartGame
    | NextLevel
    | UpdateIncorrectSelections CardId
    | ShuffleCards GameBoard


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
            case model.gameStatus of
                GameStarted ->
                    ( { model | board = updateCardSelection id model.config.currentTargets model.board }
                    , Cmd.batch
                        [ Task.succeed (UpdateIncorrectSelections id) |> Task.perform identity
                        , Task.succeed UpdateLevelCompletion |> Task.perform identity
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

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
                        , board = updateCardsTime model.config model.config.currentTargets model.board
                      }
                    , Cmd.batch
                        [ Task.succeed UpdateGameStatus |> Task.perform identity
                        ]
                    )

                GamePaused ->
                    let
                        config =
                            model.config

                        currentLevel =
                            config.currentLevel

                        updatedLevel =
                            { currentLevel
                                | shakeTimeElapsed =
                                    if currentLevel.incorrectSelections == model.config.errorTreshold && currentLevel.shakeTimeElapsed < 2 then
                                        currentLevel.shakeTimeElapsed + 1

                                    else
                                        currentLevel.shakeTimeElapsed
                            }

                        updatedConfig =
                            { config | currentLevel = updatedLevel }
                    in
                    ( { model
                        | time = newTime
                        , config = updatedConfig
                      }
                    , Cmd.batch
                        [ if updatedLevel.shakeTimeElapsed >= 2 then
                            Random.generate ShuffleCards (shuffle model.board)

                          else
                            Cmd.none
                        ]
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

        ShuffleCards list ->
            let
                config =
                    model.config

                currentLevel =
                    config.currentLevel

                updatedLevel =
                    { currentLevel
                        | shakeTimeElapsed = 0
                        , incorrectSelections = 0
                    }

                updatedConfig =
                    { config | currentLevel = updatedLevel }
            in
            ( { model
                | gameStatus = GameStarted
                , board = resetBoardFromShuffle list
                , config = updatedConfig
              }
            , Cmd.none
            )

        UpdateIncorrectSelections id ->
            let
                config =
                    model.config

                updatedLevel =
                    { config | currentLevel = updateIncorrectSelections id model }
            in
            if updatedLevel.currentLevel.incorrectSelections == model.config.errorTreshold then
                ( { model
                    | config = updatedLevel
                    , gameStatus = GamePaused
                    , board = updateBoardToShake model.board
                  }
                , Cmd.none
                )

            else
                ( { model | config = updatedLevel }, Cmd.none )


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
        let
            level =
                config.currentLevel

            updatedLevel =
                { level
                    | level = nextLevel
                    , sneakPeakTimeElapsed = 0
                    , incorrectSelections = 0
                    , shakeTimeElapsed = 0
                    , timeElapsed = 0
                }
        in
        { config | currentLevel = updatedLevel }


updateBoardToShake : GameBoard -> GameBoard
updateBoardToShake =
    List.map
        (\card ->
            let
                conf =
                    getCardConfig card
            in
            case card of
                MatchedCard _ ->
                    card

                _ ->
                    ShakingCard conf
        )


resetBoardFromShuffle : GameBoard -> GameBoard
resetBoardFromShuffle =
    List.map
        (\card ->
            case card of
                ShakingCard c ->
                    HiddenCard { c | revealTime = 0 }

                _ ->
                    card
        )


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
            model.config.currentTargets

        allTargetsComplete =
            emoticonOf target1 model.board
                |> List.append (emoticonOf target2 model.board)
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
    if config.gameTime > 25 && config.numberOfCards < 35 then
        { config
            | gameTime = config.gameTime - 3
            , numberOfCards = config.numberOfCards + 5
        }

    else
        config


updateLevelTime : LevelConfig -> LevelConfig
updateLevelTime config =
    let
        { level, timeElapsed, sneakPeakTimeElapsed, shakeTimeElapsed, incorrectSelections } =
            config.currentLevel

        currLevel =
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

        updatedLevel =
            { currLevel
                | level = level
                , timeElapsed = time
                , sneakPeakTimeElapsed = sneakPeakTime
            }
    in
    { config | currentLevel = updatedLevel }


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


updateIncorrectSelections : CardId -> Model -> Level
updateIncorrectSelections cardId model =
    let
        level =
            model.config.currentLevel

        anyMatched =
            model.board
                |> List.any
                    (\card ->
                        let
                            config =
                                getCardConfig card
                        in
                        if config.id == cardId then
                            case card of
                                MatchedCard _ ->
                                    True

                                RevealedCard _ ->
                                    True

                                _ ->
                                    False

                        else
                            False
                    )

        updatedCount =
            if anyMatched then
                0

            else
                level.incorrectSelections + 1
    in
    { level | incorrectSelections = updatedCount }


cardIsATarget : CurrentTargets -> Card CardConfig -> Bool
cardIsATarget ( target1, target2 ) card =
    let
        config =
            getCardConfig card
    in
    List.member config.emoticon [ target1, target2 ]


generateCardList : LevelConfig -> List Data.Emoticon -> GameBoard
generateCardList config =
    List.indexedMap
        (\index emoticon ->
            RevealedCard
                { id = index
                , emoticon = emoticon
                , revealTime = 0
                }
        )


emoticonOf : Data.Emoticon -> GameBoard -> GameBoard
emoticonOf target board =
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
        let
            config =
                model.config

            updatedConfig =
                { config | currentTargets = ( emoticon1, emoticon2 ) }
        in
        ( { model | config = updatedConfig }
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


cardGenerator : Random.Generator Data.Emoticon
cardGenerator =
    Random.uniform Data.GrinningFace Data.listOfEmoticons


weightedCardGenerator : CurrentTargets -> Random.Generator Data.Emoticon
weightedCardGenerator ( emoticon1, emoticon2 ) =
    let
        list =
            Data.listOfEmoticons
                |> List.filter (\emoticon -> not (emoticon == emoticon1))
                |> List.filter (\emoticon -> not (emoticon == emoticon2))
                |> List.map
                    (\emoticon ->
                        ( 5, emoticon )
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
            [ div [ class "pure-u-1 pure-u-md-2-3 u-order-2" ]
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
            , div [ class "pure-u-1 pure-u-md-1-3" ]
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
            div [ class "c-game-start" ]
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
                [ class "c-game-over" ]
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
                [ span [ class "c-emoticon" ]
                    [ text (Data.viewEmoticon config.emoticon) ]
                ]
            , div [ class "c-card-back" ]
                [ span [ class "c-emoticon" ]
                    [ text (Data.viewEmoticon Data.MountFuji) ]
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
        , let
            emojis =
                div [ class "c-sidebar__instruction-container" ]
                    [ viewEmojiTargets model ]
          in
          case model.gameStatus of
            GameStarted ->
                emojis

            GamePaused ->
                emojis

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
    let
        ( emoticon1, emoticon2 ) =
            model.config.currentTargets
    in
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ]
            [ p [ class "c-text" ]
                [ text "Find all of the following emoticons before the time runs out!" ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (Data.viewEmoticon emoticon1) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets emoticon1 model.board) ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (Data.viewEmoticon emoticon2) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets emoticon2 model.board) ]
            ]
        ]



-- Utilities


countNumberOfTargets : Data.Emoticon -> GameBoard -> Int
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
