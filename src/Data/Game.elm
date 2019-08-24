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
