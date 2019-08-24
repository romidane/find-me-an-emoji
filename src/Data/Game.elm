module Data.Game exposing (Emoticon(..), listOfEmoticons, viewEmoticon)


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
    | MountFuji
    | LoudlyCryingFace
    | FlushedFace
    | ScreamingInFearFace
    | SmilingWithHaloFace
    | ExplodingHeadFace
    | StarStruckFace
    | PartyFace
    | CowboyHatFace
    | NoFace


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
    , LoudlyCryingFace
    , FlushedFace
    , ScreamingInFearFace
    , SmilingWithHaloFace
    , ExplodingHeadFace
    , StarStruckFace
    , PartyFace
    , CowboyHatFace
    ]


viewEmoticon : Emoticon -> String
viewEmoticon emoticon =
    case emoticon of
        NoFace ->
            ""

        GrinningFace ->
            "😀"

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

        MountFuji ->
            "🗻"

        LoudlyCryingFace ->
            "😭"

        FlushedFace ->
            "😳"

        ScreamingInFearFace ->
            "😱"

        SmilingWithHaloFace ->
            "😇"

        ExplodingHeadFace ->
            "\u{1F92F}"

        StarStruckFace ->
            "\u{1F929}"

        PartyFace ->
            "\u{1F973}"

        CowboyHatFace ->
            "\u{1F920}"
