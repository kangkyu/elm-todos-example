module Main exposing (..)

import Html exposing (Html, text, div, input)
import Html.Attributes exposing (value, class)
import Html.Events exposing (onInput)


view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ input [ onInput UpdateText, value model.text ] []
        , div [] [ text model.text ]
        ]


type alias Model =
    { text : String }


type Msg
    = UpdateText String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            { model | text = newText }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { text = "" }
        , update = update
        }
