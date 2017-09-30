module ShopShare exposing (init, subscriptions, update, view)

import Config exposing (wsAddress)
import CssHelpers exposing (activeClassIfJust)
import Debounce
import Debouncers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import JSON
import PubSub exposing (publish, processEvent)
import Ports exposing (focusItemInput)
import UpdateHelpers exposing (..)
import Task
import Types exposing (..)
import UuidHelpers exposing (itemId, uuidSeedFromInt)
import WebSocket as WS


-- MODEL


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { lists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    , listToDelete = Nothing
    , listDebouncer = Debounce.init
    , itemDebouncer = Debounce.init
    }
        ! [ publish Register ]



-- SUBSCRIPTIONS


subscriptions : a -> Sub Msg
subscriptions _ =
    WS.listen wsAddress WSMessageReceived



-- UPDATE


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateListClicked ->
            let
                ( newModel, newList ) =
                    createListWithNewUuid model
            in
                newModel ! [ CreateList newList |> publish ]

        ListTitleEdited list newTitle ->
            let
                ( newModel, newList ) =
                    replaceList_ model { list | title = newTitle }
            in
                pushListEditIntoDebouncer newModel newList

        DeleteListClicked list ->
            { model | listToDelete = Just list } ! []

        DeleteListCancelClicked ->
            { model | listToDelete = Nothing } ! []

        ConfirmDeleteListClicked ->
            case model.listToDelete of
                Nothing ->
                    model ! []

                Just list ->
                    deleteList model list ! [ DeleteList list |> publish ]

        CreateItemClicked list ->
            let
                ( newModel, newItem ) =
                    createItemWithNewUuid model list
            in
                newModel ! [ perform <| FocusItem newItem, CreateItem newItem |> publish ]

        FocusItem item ->
            model ! [ focusItemInput item ]

        ItemTextEdited item newText ->
            let
                ( newModel, newItem ) =
                    replaceItem_ model { item | text = newText }
            in
                pushItemEditIntoDebouncer newModel newItem

        ItemChecked item itemChecked ->
            let
                ( newModel, newItem ) =
                    replaceItem_ model { item | completed = itemChecked }
            in
                newModel ! [ UpdateItem newItem |> publish ]

        DeleteItemClicked item ->
            deleteItem model item
                ! [ DeleteItem item |> publish ]

        ClearCheckedItems list ->
            deleteItems model .completed list ! []

        WSMessageReceived message ->
            processWSMessage model message

        DebounceListTitle debounceMsg ->
            performDebouncedListEdit model debounceMsg <| UpdateList >> publish

        DebounceItemText debounceMsg ->
            performDebouncedItemEdit model debounceMsg <| UpdateItem >> publish


processWSMessage : Model -> String -> ( Model, Cmd Msg )
processWSMessage model message =
    case JSON.decodeEvent message of
        Ok event ->
            processEvent model event

        Err err ->
            { model | errorMessage = Just err } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ pageTitle
        , section [ class "section" ]
            [ deleteListModal model
            , div [ class "container" ]
                [ createListButton
                , viewShoppingLists model
                ]
            ]
        ]


pageTitle : Html Msg
pageTitle =
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ] [ pageHeader ] ]


pageHeader : Html Msg
pageHeader =
    div [ class "container" ]
        [ h1 [ class "title" ]
            [ text "Welcome to shop share!" ]
        , h2 [ class "subtitle" ]
            [ text "Collaborative shopping in Elm & Haskell" ]
        ]


createListButton : Html Msg
createListButton =
    div [ class "section is-horizontally-centered" ]
        [ a
            [ class "button is-primary is-outlined"
            , onClick CreateListClicked
            ]
            [ span []
                [ text "New list" ]
            , span [ class "icon is-small" ]
                [ i [ class "fa fa-cart-plus" ] [] ]
            ]
        ]


viewShoppingLists : Model -> Html Msg
viewShoppingLists model =
    section [ class "section" ]
        [ div [ class "columns is-variable is-8 is-centered is-multiline" ] <|
            List.map viewShoppingList model.lists
        ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    div [ class "column is-half" ]
        [ div [ class "columns is-mobile is-vertically-centered" ]
            [ div [ class "column" ]
                [ listTitle list ]
            , div [ class "column is-narrow" ]
                [ deleteListButton list ]
            ]
        , viewItems list
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ addItemButton list ]
            , div [ class "column has-gap-below" ]
                [ clearCheckedItemsButton list ]
            ]
        ]


listTitle : ShoppingList -> Html Msg
listTitle list =
    input
        [ class "input"
        , placeholder "List title"
        , ListTitleEdited list |> onInput
        , value list.title
        ]
        []


deleteListButton : ShoppingList -> Html Msg
deleteListButton list =
    button
        [ class "button delete is-large is-primary"
        , DeleteListClicked list |> onClick
        ]
        []


viewItems : ShoppingList -> Html Msg
viewItems list =
    div [ class "box is-borderless" ] <|
        List.map viewItem list.listItems


viewItem : Item -> Html Msg
viewItem item =
    div [ class "columns is-mobile is-vertically-centered" ]
        [ input
            [ class "column input is-borderless"
            , Html.Attributes.id <| itemId item
            , placeholder "Item name"
            , value item.text
            , ItemTextEdited item |> onInput
            ]
            []
        , label
            [ class "column is-narrow checkbox" ]
            [ input
                [ type_ "checkbox"
                , checked item.completed
                , ItemChecked item |> onCheck
                ]
                []
            ]
        , button
            [ class "column is-narrow button delete"
            , DeleteItemClicked item |> onClick
            ]
            []
        ]


addItemButton : ShoppingList -> Html Msg
addItemButton list =
    button
        [ class "button"
        , CreateItemClicked list |> onClick
        ]
        [ text "Add item"
        ]


clearCheckedItemsButton : ShoppingList -> Html Msg
clearCheckedItemsButton list =
    button
        [ class "button is-light is-pulled-right"
        , ClearCheckedItems list |> onClick
        ]
        [ text "clear checked items" ]


deleteListModal : Model -> Html Msg
deleteListModal model =
    div [ activeClassIfJust model.listToDelete "modal" ]
        [ div [ class "modal-background" ]
            []
        , div
            [ class "modal-content is-horizontally-centered" ]
            [ button
                -- TODO: Too big on mobile:
                [ class "button is-primary is-large"
                , ConfirmDeleteListClicked |> onClick
                ]
                [ text "Permanently delete this shopping list" ]
            ]
        , button
            [ class "modal-close is-large"
            , DeleteListCancelClicked |> onClick
            ]
            []
        ]
