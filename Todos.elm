port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (value, class, autofocus, placeholder)
import Html.Events exposing (onInput, onClick, onSubmit, onDoubleClick)


-- view


view : Model -> Html Msg
view model =
    div [ class "col-12 col-sm-6 offset-sm-3" ]
        [ todoForm model
        , div [] (List.indexedMap (viewTodo model.editing) model.todos)
        ]


todoForm : Model -> Html Msg
todoForm model =
    form [ class "row", onSubmit AddTodo ]
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


viewTodo : Maybe TodoEdit -> Int -> Todo -> Html Msg
viewTodo editing index todo =
    case editing of
        Just todoEdit ->
            if todoEdit.id == index then
                viewEditTodo todoEdit
            else
                viewNormalTodo index todo

        Nothing ->
            viewNormalTodo index todo


viewEditTodo : TodoEdit -> Html Msg
viewEditTodo todoEdit =
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ form [ onSubmit (EditSave todoEdit) ]
                [ input
                    [ class "form-control"
                    , onInput (Edit todoEdit.id)
                    , value todoEdit.text
                    ]
                    []
                ]
            ]
        ]


viewNormalTodo : Int -> Todo -> Html Msg
viewNormalTodo index todo =
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ span [ onDoubleClick (Edit index todo.content) ]
                [ text todo.content ]
            , span
                [ onClick (RemoveTodo index)
                , class "float-right"
                ]
                [ text "✖️" ]
            ]
        ]



-- model


type alias Model =
    { text : String
    , todos : List Todo
    , editing : Maybe TodoEdit
    }


type alias TodoEdit =
    { id : Int, text : String }


type alias Todo =
    { content : String
    , completed : Bool
    }



-- update


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
            let
                newTodos =
                    Todo model.text False :: model.todos
            in
                ( { model | text = "", todos = newTodos }, saveTodos newTodos )

        RemoveTodo index ->
            let
                newTodos =
                    (List.take index model.todos) ++ (List.drop (index + 1) model.todos)
            in
                ( { model | todos = newTodos }, saveTodos newTodos )

        Edit index todoText ->
            ( { model | editing = Just { id = index, text = todoText } }, Cmd.none )

        EditSave todoEdit ->
            let
                newTodos =
                    List.indexedMap
                        (\i todo ->
                            if i == todoEdit.id then
                                { todo | content = todoEdit.text }
                            else
                                todo
                        )
                        model.todos
            in
                ( { model | editing = Nothing, todos = newTodos }, saveTodos newTodos )



-- port declaration
-- (commands)


port saveTodos : List Todo -> Cmd msg



-- init


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.todos Nothing
    , Cmd.none
    )


type alias Flags =
    { todos : List Todo }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    programWithFlags
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
