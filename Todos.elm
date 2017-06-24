module Main exposing (..)

import Html exposing (Html, text, div, input, button, ul, li, span, form)
import Html.Attributes exposing (value, class, autofocus)
import Html.Events exposing (onInput, onClick, onSubmit)


view : Model -> Html Msg
view model =
    div [ class "col-12 col-sm-6 offset-sm-3" ]
        [ form [ class "row", onSubmit AddTodo ]
            [ div [ class "col-9" ]
                [ input
                    [ class "form-control"
                    , onInput UpdateText
                    , value model.text
                    , autofocus True
                    ]
                    []
                ]
            , div [ class "col-3" ]
                [ button
                    [ class "btn btn-primary form-control" ]
                    [ text "+" ]
                ]
            ]
        , div [ class "row" ]
            [ ul [ class "col-12" ] (List.indexedMap itemView model.todos) ]
        ]


itemView : Int -> String -> Html Msg
itemView index todo =
    li [ class "card" ]
        [ div [ class "card-block" ]
            [ text todo
            , span
                [ onClick (RemoveTodo index)
                , class "float-right"
                ]
                [ text "✖️" ]
            ]
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
