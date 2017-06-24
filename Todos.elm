module Main exposing (..)

import Html exposing (Html, text, div, input, button, ul, li)
import Html.Attributes exposing (value, class)
import Html.Events exposing (onInput, onClick)


view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ input [ onInput UpdateText, value model.text ] []
        , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Add" ]
        , ul [] (List.map (\todo -> li [] [ text todo ]) model.todos)
        ]


type alias Model =
    { text : String, todos : List String }


type Msg
    = UpdateText String
    | AddTodo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            { model | text = newText }

        AddTodo ->
            { model | text = "", todos = model.text :: model.todos }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { text = "", todos = [] }
        , update = update
        }
