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
    div [] [ text model.title ]
