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
            [ div [ class "pure-u-1 pure-u-md-2-3" ]
                [ h3 [ class "c-heading-alpha" ] [ text "About Find me" ]
                , p [] [ text "I started writting this game because I wanted to get some practice using elm." ]
                , p [] [ text "I wanted to experience how hard it would be write a single page application as well as do something that would invlove me using a lot of the Api's elm has to offer" ]
                ]
            ]
        ]
