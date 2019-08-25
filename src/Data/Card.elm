module Data.Card exposing (Card(..), CardConfig, CardId, getCardConfig, shakeCard, updateCardMatching, updateCardTime)

import Data.Emoticon exposing (Emoticon)



-- Models


type Card config
    = RevealedCard config
    | SelectedCard config
    | HiddenCard config
    | MatchedCard config
    | ShakingCard config


type alias CardId =
    Int


type alias EmoticonTargets =
    ( Emoticon, Emoticon )


type alias CardConfig =
    { id : CardId
    , emoticon : Emoticon
    , revealTime : Int
    }



-- Update


shakeCard : Card CardConfig -> Card CardConfig
shakeCard card =
    let
        conf =
            getCardConfig card
    in
    case card of
        MatchedCard _ ->
            card

        _ ->
            ShakingCard conf


updateCardMatching : CardId -> EmoticonTargets -> Card CardConfig -> Card CardConfig
updateCardMatching id targets card =
    let
        config =
            getCardConfig card
    in
    if config.id == id then
        case card of
            RevealedCard _ ->
                card

            HiddenCard c ->
                if cardIsATarget targets card then
                    MatchedCard { c | revealTime = 0 }

                else
                    SelectedCard c

            _ ->
                card

    else
        card


cardIsATarget : EmoticonTargets -> Card CardConfig -> Bool
cardIsATarget ( target1, target2 ) card =
    let
        config =
            getCardConfig card
    in
    List.member config.emoticon [ target1, target2 ]


updateCardTime : Int -> Card CardConfig -> Card CardConfig
updateCardTime sneakPeakTime card =
    let
        cardConfig =
            getCardConfig card

        timeExpired =
            cardConfig.revealTime >= sneakPeakTime

        resetRevealTime =
            { cardConfig | revealTime = 0 }

        updatedRevealTime =
            { cardConfig | revealTime = cardConfig.revealTime + 1 }
    in
    case card of
        RevealedCard _ ->
            if timeExpired then
                HiddenCard resetRevealTime

            else
                RevealedCard updatedRevealTime

        SelectedCard _ ->
            if timeExpired then
                HiddenCard resetRevealTime

            else
                SelectedCard updatedRevealTime

        _ ->
            card



-- Utilities


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
