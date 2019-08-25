module Page.About exposing (Model, Msg, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = "About" }
    , Cmd.none
    )


type alias Model =
    { title : String
    }


type Msg
    = NONE



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "o-page" ]
        [ div [ class "pure-g" ]
            [ div [ class "pure-u-1 pure-u-md-2-3" ]
                [ h3 [ class "c-heading-alpha" ]
                    [ text "Some Batman Ipsum" ]
                , p
                    []
                    [ text "Blue knight superman diamond shiva basil ragdoll czonk mask, bane scorn. Firebug oracle shade green faith charlatan checkmate czonk moth bane. Clayface alcor ragman rose rupert oracle. Hammer poison scarecrow martha deathstroke dick supergirl nyssa. Aquaman aquaman joker gotham thorne batmobile jason echo cluemaster kyle scarecrow zeus. Green blake arrow deathstroke edward harlequin wayne pennyworth a pit harvey abbott? Mugsy hush grundy al calendar boomerang? Falcone gearhead joe." ]
                , p []
                    [ text "Todd penguin caird lantern selina barrow elongated flash fairchild poison elongated! Gotham nyssa, arrow batman edward. Gearhead temblor oracle ragman. Wing bane america blink anarky atomic! Raatko ragdoll, bruce wayne rhino a barrow kobra. Of atom lynx moth checkmate ivy, clock young young mad canary martha? Harlequin vale ragman smoke fairchild. Black maxie a lazarus! America chase dent thomas montoya! Zeus joker." ]
                , p []
                    [ text "Young aquaman arrow green black shadow. Black night wing kane calendar aquaman smoke temblor anarky. Face prey riddler luthor grey superman clayface aquaman. Tumbler diamond oswald blake swamp chill crane rhino azrael arrow rose. Kobra, diamond firebug gearhead snake ra smoke. Pit zeus clock harley jim riddler grey blink. Drake crane toymaker barbara spoiler thomas clench sinestro! Boomerang riddler hangman cain snake basil, freeze pennyworth batgirl hood sinestro gleeson. Superman grey lex society damian thorne killer robin? Faith shiva mad clock deathstroke!" ]
                ]
            ]
        ]
