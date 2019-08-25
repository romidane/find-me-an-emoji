module Data.Game exposing
    ( Game
    , GameBoard
    , Level
    , LevelConfig
    , initialGame
    , resetConfigFromShuffle
    , updateBoardToShake
    , updateCardSelection
    , updateCardsTime
    , updateGameBoard
    , updateGameLevel
    , updateIncorrectSelectionCount
    , updateLevelShakeTimeElapsed
    , updateLevelTargetsWith
    , updateLevelTime
    , updatedGameBoardAfterShake
    )

import Data.Card exposing (..)
import Data.Emoticon exposing (Emoticon, blankEmoticon)



-- Models


type alias CurrentTargets =
    ( Emoticon, Emoticon )


type alias Game =
    { board : GameBoard
    , levelConfig : LevelConfig
    , level : Level
    }


type alias LevelConfig =
    { gameTime : Int
    , errorThreshold : Int
    , sneakPeakTime : Int
    , numberOfCards : Int
    , numberOfTargets : Int
    }


type alias Level =
    { number : Int
    , timeElapsed : Int
    , sneakPeakTimeElapsed : Int
    , incorrectSelections : Int
    , shakeTimeElapsed : Int
    , currentTargets : CurrentTargets
    }


type alias GameBoard =
    List (Card CardConfig)


initialGame : Game
initialGame =
    { board = []
    , levelConfig =
        { gameTime = 45
        , errorThreshold = 5
        , sneakPeakTime = 4
        , numberOfCards = 25
        , numberOfTargets = 3
        }
    , level =
        { number = 1
        , timeElapsed = 0
        , sneakPeakTimeElapsed = 0
        , incorrectSelections = 0
        , shakeTimeElapsed = 0
        , currentTargets = ( blankEmoticon, blankEmoticon )
        }
    }



-- Update


updateGameLevel : Game -> Game
updateGameLevel game =
    let
        currentLevel =
            game.level

        currentLevelConfig =
            game.levelConfig

        updatedLevel =
            { currentLevel | number = currentLevel.number + 1 }
    in
    if modBy updatedLevel.number 5 == 0 then
        let
            updateLevelConfig =
                { currentLevelConfig | numberOfCards = currentLevelConfig.numberOfCards + 5 }
        in
        { game
            | level = updatedLevel
            , levelConfig = updateLevelConfig
        }

    else
        let
            updatedLevelReset =
                { updatedLevel
                    | sneakPeakTimeElapsed = 0
                    , incorrectSelections = 0
                    , shakeTimeElapsed = 0
                    , timeElapsed = 0
                }

            updatedLevelConfig =
                { currentLevelConfig
                    | gameTime =
                        if currentLevelConfig.gameTime > 30 then
                            currentLevelConfig.gameTime - 3

                        else
                            currentLevelConfig.gameTime
                }
        in
        { game
            | level = updatedLevelReset
            , levelConfig = updatedLevelConfig
        }


updateBoardToShake : Game -> Game
updateBoardToShake game =
    { game | board = game.board |> List.map shakeCard }


updateLevelShakeTimeElapsed : Game -> Game
updateLevelShakeTimeElapsed game =
    let
        currentLevel =
            game.level

        errorLimitReached =
            currentLevel.incorrectSelections == game.levelConfig.errorThreshold

        shakeTimeLeft =
            currentLevel.shakeTimeElapsed < 2

        updatedLevel =
            { currentLevel
                | shakeTimeElapsed =
                    if errorLimitReached && shakeTimeLeft then
                        currentLevel.shakeTimeElapsed + 1

                    else
                        currentLevel.shakeTimeElapsed
            }
    in
    { game | level = updatedLevel }


isLevelComplete : Game -> Bool
isLevelComplete { level, board } =
    let
        ( target1, target2 ) =
            level.currentTargets
    in
    listContainingEmoticon target1 board
        |> List.append (listContainingEmoticon target2 board)
        |> List.all
            (\card ->
                case card of
                    MatchedCard _ ->
                        True

                    _ ->
                        False
            )


updateLevelTime : Game -> Game
updateLevelTime game =
    let
        { number, timeElapsed, sneakPeakTimeElapsed, shakeTimeElapsed, incorrectSelections } =
            game.level

        { gameTime, sneakPeakTime } =
            game.levelConfig

        currentLevel =
            game.level

        updatedTimeElapsed =
            if timeElapsed < gameTime && sneakPeakTimeElapsed >= sneakPeakTime then
                timeElapsed + 1

            else
                timeElapsed

        updatedSneakPeakTimeElapsed =
            if sneakPeakTimeElapsed < sneakPeakTime then
                sneakPeakTimeElapsed + 1

            else
                sneakPeakTimeElapsed

        updatedLevel =
            { currentLevel
                | timeElapsed = updatedTimeElapsed
                , sneakPeakTimeElapsed = updatedSneakPeakTimeElapsed
            }
    in
    { game | level = updatedLevel }


updateCardsTime : Game -> Game
updateCardsTime game =
    let
        { sneakPeakTime } =
            game.levelConfig

        board =
            game.board |> List.map (updateCardTime sneakPeakTime)
    in
    { game | board = board }


updateCardSelection : CardId -> Game -> Game
updateCardSelection id game =
    { game
        | board =
            game.board
                |> List.map (updateCardMatching id game.level.currentTargets)
    }


updateIncorrectSelectionCount : CardId -> Game -> Game
updateIncorrectSelectionCount cardId game =
    let
        level =
            game.level

        anyMatched =
            game.board
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

        updatedLevel =
            { level | incorrectSelections = updatedCount }
    in
    { game | level = updatedLevel }


updatedGameBoardAfterShake : Game -> GameBoard -> Game
updatedGameBoardAfterShake game board =
    let
        { sneakPeakTime } =
            game.levelConfig

        updatedBoard =
            board
                |> List.map
                    (\card ->
                        case card of
                            ShakingCard c ->
                                RevealedCard { c | revealTime = sneakPeakTime // 2 }

                            _ ->
                                card
                    )
    in
    { game | board = updatedBoard }


updateLevelTargetsWith : CurrentTargets -> Game -> Game
updateLevelTargetsWith targets game =
    let
        level =
            game.level

        updatedLevel =
            { level | currentTargets = targets }
    in
    { game | level = updatedLevel }


updateGameBoard : Game -> List Emoticon -> Game
updateGameBoard game list =
    { game
        | board =
            list
                |> List.indexedMap
                    (\index emoticon ->
                        RevealedCard
                            { id = index
                            , emoticon = emoticon
                            , revealTime = 0
                            }
                    )
    }


updateGameLevelConfig : Game -> Game
updateGameLevelConfig game =
    let
        levelConfig =
            game.levelConfig

        updatedLevelConfig =
            if levelConfig.gameTime > 25 && levelConfig.numberOfCards < 35 then
                { levelConfig
                    | gameTime = levelConfig.gameTime - 3
                    , numberOfCards = levelConfig.numberOfCards + 5
                }

            else
                levelConfig
    in
    { game | levelConfig = updatedLevelConfig }


resetConfigFromShuffle : Game -> Game
resetConfigFromShuffle game =
    let
        currentLevel =
            game.level

        updatedLevel =
            { currentLevel
                | shakeTimeElapsed = 0
                , incorrectSelections = 0
            }
    in
    { game | level = updatedLevel }



-- Utilities


listContainingEmoticon : Emoticon -> GameBoard -> GameBoard
listContainingEmoticon target board =
    board
        |> List.filter
            (\card ->
                let
                    config =
                        getCardConfig card
                in
                config.emoticon == target
            )
