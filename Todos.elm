module Main exposing (..)

import Html exposing (Html, text, div, input, button, ul, li, span, form)
import Html.Attributes exposing (value, class, autofocus, placeholder)
import Html.Events exposing (onInput, onClick, onSubmit, onDoubleClick)


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
                    , placeholder "Enter a todo"
                    ]
                    []
                ]
            , div [ class "col-3" ]
                [ button
                    [ class "btn btn-primary form-control" ]
                    [ text "+" ]
                ]
            ]
        , div [] (List.indexedMap (viewTodo model.editing) model.todos)
        ]


viewTodo : Maybe TodoEdit -> Int -> String -> Html Msg
viewTodo editing index todo =
    case editing of
        Just todoEdit ->
            if todoEdit.id == index then
                div [ class "card" ]
                    [ div [ class "card-block" ]
                        [ form [ onSubmit (EditSave todoEdit) ]
                            [ input
                                [ class "form-control"
                                , onInput (Edit index)
                                , value todoEdit.text
                                ]
                                []
                            ]
                        ]
                    ]
            else
                viewNormalTodo index todo

        Nothing ->
            viewNormalTodo index todo


viewNormalTodo : Int -> String -> Html Msg
viewNormalTodo index todo =
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ span [ onDoubleClick (Edit index todo) ]
                [ text todo ]
            , span
                [ onClick (RemoveTodo index)
                , class "float-right"
                ]
                [ text "✖️" ]
            ]
        ]


type alias Model =
    { text : String, todos : List String, editing : Maybe TodoEdit }


type alias TodoEdit =
    { id : Int, text : String }


type Msg
    = UpdateText String
    | AddTodo
    | RemoveTodo Int
    | Edit Int String
    | EditSave TodoEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText newText ->
            ( { model | text = newText }, Cmd.none )

        AddTodo ->
            ( { model | text = "", todos = model.text :: model.todos }, Cmd.none )

        RemoveTodo index ->
            let
                newTodos =
                    (List.take index model.todos) ++ (List.drop (index + 1) model.todos)
            in
                ( { model | todos = newTodos }, Cmd.none )

        Edit index todoText ->
            ( { model | editing = Just { id = index, text = todoText } }, Cmd.none )

        EditSave todoEdit ->
            let
                newTodos =
                    List.indexedMap
                        (\i todo ->
                            if i == todoEdit.id then
                                todoEdit.text
                            else
                                todo
                        )
                        model.todos
            in
                ( { model | editing = Nothing, todos = newTodos }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { text = "", todos = [ "Laundry", "Dishes" ], editing = Nothing }, Cmd.none )
