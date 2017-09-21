module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import List.Extra exposing (..)
import WebSocket as WS
import Action exposing (publishAction)
import Config exposing (wsAddress)
import Event exposing (..)
import JSON
import Types exposing (..)
import UpdateHelpers exposing (..)
import UuidHelpers exposing (..)


-- MODEL


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { shoppingLists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    , listToDelete = Nothing
    }
        ! [ publishAction Register ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen wsAddress WSMessageReceived



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateListClicked ->
            let
                ( newModel, newList ) =
                    createListWithNewUuid model
            in
                newModel ! [ publishAction <| CreateList newList ]

        DeleteListClicked list ->
            { model | listToDelete = Just list } ! []

        DeleteListConfirmClicked ->
            case model.listToDelete of
                Nothing ->
                    model ! []

                Just list ->
                    let
                        updatedModel =
                            deleteList model list
                    in
                        { updatedModel | listToDelete = Nothing }
                            ! [ publishAction <| DeleteList list ]

        DeleteListCancelClicked ->
            { model | listToDelete = Nothing } ! []

        CreateItemClicked list ->
            let
                ( newModel, newItem ) =
                    createItemWithNewUuid model list
            in
                newModel ! [ publishAction <| CreateItem newItem ]

        ListTitleEdited list newTitle ->
            let
                ( newModel, updatedList ) =
                    editListTitle model list newTitle
            in
                newModel ! [ publishAction <| UpdateList updatedList ]

        ItemTextEdited list item newItemText ->
            let
                updatedItem =
                    { item | text = newItemText }
            in
                replaceList (updateItem updatedItem list) model
                    ! [ publishAction <| UpdateItem updatedItem ]

        ItemChecked list item itemChecked ->
            let
                updatedItem =
                    { item | completed = itemChecked }
            in
                replaceList (updateItem updatedItem list) model
                    ! [ publishAction <| UpdateItem updatedItem ]

        DeleteItemClicked list deletedItem ->
            replaceList (deleteItem deletedItem list) model
                ! [ publishAction <| DeleteItem deletedItem ]

        ClearCheckedItems list ->
            let
                updatedList =
                    { list
                        | listItems = List.Extra.filterNot .completed list.listItems
                    }
            in
                replaceList updatedList model ! []

        WSMessageReceived message ->
            handleMessage model message


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    case JSON.decodeEvent message of
        Ok event ->
            handleEvent model event

        Err err ->
            { model | errorMessage = Just err } ! []



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ deleteListModal model
        , div
            [ class "container" ]
            [ viewPageTitle
            , viewCreateListButton
            , viewShoppingLists model
            , viewErrors model
            , viewClientId model
            ]
        ]


viewPageTitle : Html Msg
viewPageTitle =
    div [ class "section" ] [ h1 [ class "title" ] [ text "Welcome to shop share!" ] ]


viewCreateListButton : Html Msg
viewCreateListButton =
    div [ class "section is-horizontally-centered" ]
        [ button
            [ class "button is-primary"
            , onClick CreateListClicked
            ]
            [ text "New list" ]
        ]


viewShoppingLists : Model -> Html Msg
viewShoppingLists model =
    div [ class "section columns is-centered is-multiline" ] <|
        List.map viewShoppingList <|
            model.shoppingLists


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    div [ class "column is-half" ]
        [ viewListTitleAndDeleteButton list
        , viewListItems list
        , viewClearCheckedItemsButton list
        ]


viewListTitleAndDeleteButton : ShoppingList -> Html Msg
viewListTitleAndDeleteButton list =
    div [ class "columns is-vertically-centered" ]
        [ input
            [ class "column is-11 input"
            , placeholder "List title"
            , onInput <| ListTitleEdited list
            , value list.title
            ]
            []
        , button
            [ class "column is-1 button delete is-large is-danger is-pulled-right"
            , onClick <| DeleteListClicked list
            ]
            []
        ]


viewListItems : ShoppingList -> Html Msg
viewListItems list =
    div []
        [ div [ class "list" ] <|
            List.map (viewListItem list) list.listItems
                ++ [ viewAddListItem list ]
        ]


viewListItem : ShoppingList -> Item -> Html Msg
viewListItem list item =
    -- FIXME: Vertical centering not working for small screens:
    div [ class "columns is-vertically-centered" ]
        [ input
            [ Html.Attributes.id <| itemId item
            , class "column is-10 input is-borderless"
            , placeholder "Item name"
            , value item.text
            , onInput <| ItemTextEdited list item
            ]
            []
        , label [ class "column is-1 checkbox" ]
            [ input
                [ type_ "checkbox"
                , checked item.completed
                , Html.Events.onCheck <| ItemChecked list item
                ]
                []
            ]
        , button
            [ class "column is-1 button delete is-large"
            , onClick <| DeleteItemClicked list item
            ]
            []
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    dd []
        [ input
            [ class "input has-shadow-only"
            , placeholder "Add item"
            , onClick <| CreateItemClicked list
            , onFocus <| CreateItemClicked list
            ]
            []
        ]


viewClearCheckedItemsButton : ShoppingList -> Html Msg
viewClearCheckedItemsButton list =
    button
        [ class "button is-light is-pulled-right"
        , onClick <| ClearCheckedItems list
        ]
        [ text "clear checked items" ]


deleteListModal : Model -> Html Msg
deleteListModal model =
    let
        modalClass =
            case model.listToDelete of
                Nothing ->
                    "modal"

                Just list ->
                    "modal is-active"
    in
        div [ class modalClass ]
            [ div [ class "modal-background" ] []
            , div
                [ class "modal-content is-horizontally-centered" ]
                [ button
                    [ class "button is-danger"
                    , onClick DeleteListConfirmClicked
                    ]
                    [ text "Permanently delete this shopping list" ]
                ]
            , button
                [ class "modal-close is-large"
                , onClick DeleteListCancelClicked
                ]
                []
            ]


viewErrors : Model -> Html Msg
viewErrors model =
    div [] [ text <| Maybe.withDefault "" model.errorMessage ]


viewClientId : Model -> Html Msg
viewClientId model =
    case model.clientId of
        Nothing ->
            div [] []

        Just id ->
            h3 [] [ text <| "Registered with server as client " ++ toString id ]
