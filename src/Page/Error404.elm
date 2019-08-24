module Page.Error404 exposing (view)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)


view : Html msg
view =
    div [ class "o-page" ]
        [ div [ class "pure-g" ]
            [ div
                [ class "pure-u-1 pure-u-md-3-4" ]
                [ h1 [ class "c-heading-alpha" ]
                    [ text "The page you are looking for does not exist" ]
                , a [ href "/" ] [ text "Return home" ]
                ]
            ]
        ]
