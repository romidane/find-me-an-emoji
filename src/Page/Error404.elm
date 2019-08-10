module Page.Error404 exposing (view)

import Html exposing (Html, div, text)


view : Html msg
view =
    div [] [ text "The page you are looking for does not exit" ]
