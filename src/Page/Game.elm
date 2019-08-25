module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Data.Card exposing (..)
import Data.Emoticon exposing (Emoticon, defaultEmoticon, emoticonGenerator, inactiveEmoticon, listOfEmoticons)
import Data.Game exposing (..)
import Html exposing (Html, aside, button, div, h2, p, section, span, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import List
import List.Extra as ListExtra
import Random
import Random.List exposing (shuffle)
import String
import Task
import Time



-- MODEL


type GameStatus
    = GameStarted
    | GameIdle
    | GameOver
    | GameCutScene


type alias CurrentTargets =
    ( Emoticon, Emoticon )


type alias Model =
    { game : Game
    , time : Time.Posix
    , zone : Time.Zone
    , status : GameStatus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = initialGame
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , status = GameIdle
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
            ( { model | game = initialGame, status = GameIdle }
            , Cmd.batch
                [ generateRandomPair
                ]
            )

        NextLevel ->
            ( { model | game = updateGameLevel model.game, status = GameIdle }
            , Cmd.batch
                [ generateRandomPair
                ]
            )

        StartGame ->
            ( { model | status = GameStarted }, Cmd.none )

        SelectCard id ->
            case model.status of
                GameStarted ->
                    ( { model | game = updateCardSelection id model.game }
                    , Cmd.batch
                        [ Task.succeed (UpdateIncorrectSelections id) |> Task.perform identity
                        , Task.succeed UpdateLevelCompletion |> Task.perform identity
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        NewCards list ->
            ( { model | game = updateGameBoard model.game list }
            , Cmd.none
            )

        ShuffleCards list ->
            ( { model
                | status = GameStarted
                , game = updatedGameBoardAfterShake model.game list |> resetConfigFromShuffle
              }
            , Cmd.none
            )

        Tick newTime ->
            case model.status of
                GameStarted ->
                    ( { model
                        | time = newTime
                        , game = model.game |> updateLevelTime |> updateCardsTime
                      }
                    , Task.succeed UpdateGameStatus |> Task.perform identity
                    )

                GameCutScene ->
                    let
                        updatedGame =
                            updateLevelShakeTimeElapsed model.game
                    in
                    ( { model
                        | time = newTime
                        , game = updatedGame
                      }
                    , if updatedGame.level.shakeTimeElapsed >= 2 then
                        Random.generate ShuffleCards (shuffle model.game.board)

                      else
                        Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        PairOfCards ( emoticon1, emoticon2 ) ->
            if emoticon1 == emoticon2 then
                ( model, generateRandomPair )

            else
                let
                    updatedGame =
                        model.game |> updateLevelTargetsWith ( emoticon1, emoticon2 )
                in
                ( { model | game = updatedGame }, generateNewCards updatedGame )

        UpdateLevelCompletion ->
            if isLevelComplete model.game then
                ( model, Task.succeed NextLevel |> Task.perform identity )

            else
                ( model, Cmd.none )

        UpdateGameStatus ->
            ( { model | status = updateGameStatus model }
            , Cmd.none
            )

        UpdateIncorrectSelections id ->
            let
                game =
                    model.game

                updatedGame =
                    updateIncorrectSelectionCount id model.game
            in
            if updatedGame.level.incorrectSelections == updatedGame.levelConfig.errorThreshold then
                ( { model
                    | game = updatedGame |> updateBoardToShake
                    , status = GameCutScene
                  }
                , Cmd.none
                )

            else
                ( { model | game = updatedGame }, Cmd.none )


updateGameStatus : Model -> GameStatus
updateGameStatus { game, status } =
    let
        { level, levelConfig } =
            game
    in
    if level.timeElapsed >= levelConfig.gameTime then
        GameOver

    else
        status


generateRandomPair : Cmd Msg
generateRandomPair =
    Random.generate PairOfCards <| Random.pair emoticonGenerator emoticonGenerator


generateNewCards : Game -> Cmd Msg
generateNewCards game =
    let
        { currentTargets } =
            game.level

        { numberOfCards, numberOfTargets } =
            game.levelConfig

        ( emoticon1, emoticon2 ) =
            currentTargets
    in
    Random.generate NewCards
        (Random.list (numberOfCards - (numberOfTargets * 2)) (weightedCardGenerator ( emoticon1, emoticon2 ))
            |> Random.andThen
                (\weightedList ->
                    let
                        list1 =
                            List.repeat numberOfTargets emoticon1

                        list2 =
                            List.repeat numberOfTargets emoticon2

                        newList =
                            weightedList
                                |> ListExtra.interweave list1
                                |> List.reverse
                                |> ListExtra.interweave list2
                    in
                    newList |> shuffle
                )
        )


weightedCardGenerator : CurrentTargets -> Random.Generator Emoticon
weightedCardGenerator ( emoticon1, emoticon2 ) =
    let
        list =
            listOfEmoticons
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
                        (model.game.board
                            |> List.map
                                (\card ->
                                    section [ class "pure-u-1-5" ]
                                        [ lazy viewCard card ]
                                )
                        )
                    , viewGameOver model.status
                    ]
                ]
            , div [ class "pure-u-1 pure-u-md-1-3" ]
                [ viewSidebar model ]
            ]
        ]


viewStart : Model -> Html Msg
viewStart model =
    let
        { number } =
            model.game.level
    in
    case model.status of
        GameIdle ->
            div [ class "c-game-start" ]
                [ div [ class "c-game-start__content" ]
                    [ h2 [ class "c-heading-bravo u-no-margin" ] [ text <| "Level: " ++ String.fromInt number ]
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
            viewCardItem "is-selected" config

        HiddenCard config ->
            viewCardItem "is-flipped" config

        RevealedCard config ->
            viewCardItem "" config

        MatchedCard config ->
            viewCardItem "is-matched" config

        ShakingCard config ->
            viewCardItem "is-flipped is-shaking" config


viewCardItem : String -> CardConfig -> Html Msg
viewCardItem cssClasses config =
    div
        [ class ("c-card " ++ cssClasses)
        , id (String.fromInt config.id)
        , onClick (SelectCard config.id)
        ]
        [ div [ class "c-card-inner" ]
            [ div [ class "c-card-front" ]
                [ span [ class "c-emoticon" ]
                    [ text (Data.Emoticon.toString config.emoticon) ]
                ]
            , div [ class "c-card-back" ]
                [ span [ class "c-emoticon" ]
                    [ text (Data.Emoticon.toString inactiveEmoticon) ]
                ]
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        { number } =
            model.game.level
    in
    aside [ class "c-sidebar" ]
        [ div [ class "pure-g u-mb-bravo" ]
            [ div [ class "pure-u-1-2" ]
                [ h2
                    [ class "c-heading-bravo u-no-margin" ]
                    [ text ("Level: " ++ String.fromInt number) ]
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
          case model.status of
            GameStarted ->
                emojis

            GameCutScene ->
                emojis

            _ ->
                text ""
        ]


viewTimer : Model -> Html msg
viewTimer { game } =
    let
        { timeElapsed } =
            game.level

        { gameTime } =
            game.levelConfig

        countDown =
            (toFloat timeElapsed / toFloat gameTime) * 100
    in
    div [ class "c-timer-meter" ] [ span [ style "width" (String.fromFloat countDown ++ "%") ] [] ]


viewEmojiTargets : Model -> Html msg
viewEmojiTargets { game } =
    let
        ( emoticon1, emoticon2 ) =
            game.level.currentTargets
    in
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ]
            [ p [ class "c-text" ]
                [ text "Find all of the following emoticons before the time runs out!" ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (Data.Emoticon.toString emoticon1) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets emoticon1 game.board) ]
            ]
        , div [ class "pure-u-1-2" ]
            [ span [ class "c-icon__emoticon" ]
                [ text (Data.Emoticon.toString emoticon2) ]
            , span [ class "c-icon__badge" ]
                [ text (String.fromInt <| countNumberOfTargets emoticon2 game.board) ]
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
