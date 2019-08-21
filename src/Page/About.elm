module Page.About exposing (Model, Msg(..), init, view)

import Html exposing (..)
import Html.Attributes exposing (..)



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = "Likedy split" }
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
            [ div [ class "pure-u-2-3" ]
                [ h3 [ class "c-heading-alpha" ] [ text "About Find me" ]
                , p [] [ text "I started writting this game because I wanted to get some practice using elm." ]
                , p [] [ text "The benefits of writting a game is that you can take it as far as you want" ]
                , p [] [ text "But you'd always want your game to be playable" ]
                ]
            ]
        ]
