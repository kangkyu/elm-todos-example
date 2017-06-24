module Main exposing (..)

import Html exposing (Html, text, div, input, button, ul, li, span)
import Html.Attributes exposing (value, class)
import Html.Events exposing (onInput, onClick)


view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ input [ onInput UpdateText, value model.text ] []
        , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Add" ]
        , ul [] (List.indexedMap itemView model.todos)
        ]


itemView : Int -> String -> Html Msg
itemView index todo =
    li []
        [ text todo
        , span [ onClick (RemoveTodo index) ] [ text " X" ]
        ]


type alias Model =
    { text : String, todos : List String }


type Msg
    = UpdateText String
    | AddTodo
    | RemoveTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            { model | text = newText }

        AddTodo ->
            { model | text = "", todos = model.text :: model.todos }

        RemoveTodo index ->
            let
                newTodos =
                    (List.take index model.todos) ++ (List.drop (index + 1) model.todos)
            in
                { model | todos = newTodos }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { text = "", todos = [] }
        , update = update
        }
