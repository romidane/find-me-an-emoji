module Page.Home exposing (Model, Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)



-- MODEL


type alias Model =
    { title : String
    }


type Msg
    = NONE



-- VIEW
-- view : Model -> Html Msg


view model msg =
    div [] [ text model.title ]
