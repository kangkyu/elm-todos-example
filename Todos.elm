module Main exposing (..)

import Html exposing (text, div, input)
import Html.Attributes exposing (value, class)


view model =
    div [ class "text-center" ]
        [ input [ value "Hello, world" ] []
        , div [] [ text "Hello, world" ]
        ]


update model =
    model


main =
    Html.beginnerProgram
        { view = view
        , model = { text = "" }
        , update = update
        }
