module Data.Emoticon exposing
    ( Emoticon
    , blankEmoticon
    , defaultEmoticon
    , emoticonGenerator
    , inactiveEmoticon
    , listOfEmoticons
    , toString
    )

import Random


type Emoticon
    = Emoticon String


listOfEmoticons : List Emoticon
listOfEmoticons =
    List.map Emoticon
        [ "😂"
        , "😀"
        , "😏"
        , "😤"
        , "😌"
        , "😚"
        , "😍"
        , "😒"
        , "😪"
        , "😨"
        , "😝"
        , "😵"
        , "😭"
        , "😳"
        , "😱"
        , "😱"
        , "😇"
        , "\u{1F92F}"
        , "\u{1F929}"
        , "\u{1F973}"
        , "\u{1F920}"
        ]


blankEmoticon : Emoticon
blankEmoticon =
    Emoticon ""


inactiveEmoticon : Emoticon
inactiveEmoticon =
    Emoticon "🗻"


defaultEmoticon : Emoticon
defaultEmoticon =
    Emoticon "😀"


toString : Emoticon -> String
toString (Emoticon str) =
    str


emoticonGenerator : Random.Generator Emoticon
emoticonGenerator =
    Random.uniform defaultEmoticon listOfEmoticons
