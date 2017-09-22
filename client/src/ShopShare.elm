module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import List.Extra
import WebSocket as WS
import Action exposing (publishAction)
import Config exposing (wsAddress)
import Event exposing (..)
import JSON
import Types exposing (..)
import UpdateHelpers exposing (..)
import UuidHelpers exposing (..)
import Debounce
import Time


-- MODEL


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { lists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    , listToDelete = Nothing
    , debounce = Debounce.init
    }
        ! [ publishAction Register ]


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later <| 0.5 * Time.second
    , transform = DebounceMsg
    }



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

                ( newDebounce, cmd ) =
                    Debounce.push debounceConfig updatedList model.debounce
            in
                { newModel | debounce = newDebounce } ! [ cmd ]

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

        DebounceMsg msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast sync)
                        msg
                        model.debounce
            in
                { model | debounce = debounce } ! [ cmd ]


sync : ShoppingList -> Cmd Msg
sync list =
    publishAction <| UpdateList list


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    case JSON.decodeEvent message of
        Ok event ->
            handleEvent model (Debug.log "Event: " event)

        Err err ->
            { model | errorMessage = Just err } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewPageTitle
        , section [ class "section" ]
            [ viewDeleteListModal model
            , div [ class "container" ]
                [ viewCreateListButton
                , viewShoppingLists model
                  -- , viewErrors model
                  -- , viewClientId model
                ]
            ]
        ]


viewPageTitle : Html Msg
viewPageTitle =
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text "Welcome to shop share!" ]
                , h2 [ class "subtitle" ]
                    [ text "Collaborative shopping in Elm & Haskell" ]
                ]
            ]
        ]


viewCreateListButton : Html Msg
viewCreateListButton =
    div [ class "section is-horizontally-centered" ]
        [ a
            [ class "button is-primary is-outlined"
            , onClick CreateListClicked
            ]
            [ span []
                [ text "New list" ]
            , span [ class "icon is-small" ]
                [ i [ class "fa fa-cart-plus" ]
                    []
                ]
            ]
        ]


viewShoppingLists : Model -> Html Msg
viewShoppingLists model =
    section [ class "section" ]
        [ div [ class "columns is-variable is-8 is-centered is-multiline" ] <|
            List.map viewShoppingList <|
                model.lists
        ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    div [ class "column is-half" ]
        [ viewListTitleAndDeleteButton list
        , viewListItems list
        , div [ class "columns" ]
            [ viewAddListItem list
            , viewClearCheckedItemsButton list
            ]
        ]


viewListTitleAndDeleteButton : ShoppingList -> Html Msg
viewListTitleAndDeleteButton list =
    div [ class "columns is-mobile is-vertically-centered" ]
        [ div [ class "column" ]
            [ input
                [ class "input"
                , placeholder "List title"
                , onInput <| ListTitleEdited list
                , value list.title
                ]
                []
            ]
        , div [ class "column is-narrow" ]
            [ button
                [ class "button delete is-large is-primary"
                , onClick <| DeleteListClicked list
                ]
                []
            ]
        ]


viewListItems : ShoppingList -> Html Msg
viewListItems list =
    div [ class "box is-borderless" ] <|
        List.map (viewListItem list) list.listItems


viewListItem : ShoppingList -> Item -> Html Msg
viewListItem list item =
    -- FIXME: Vertical centering not working for small screens:
    div [ class "columns is-mobile is-vertically-centered" ]
        [ input
            [ Html.Attributes.id <| itemId item
            , class "column input is-borderless"
            , placeholder "Item name"
            , value item.text
            , onInput <| ItemTextEdited list item
            ]
            []
        , label [ class "column is-narrow checkbox" ]
            [ input
                [ type_ "checkbox"
                , checked item.completed
                , Html.Events.onCheck <| ItemChecked list item
                ]
                []
            ]
        , button
            [ class "column is-narrow button delete"
            , onClick <| DeleteItemClicked list item
            ]
            []
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    div [ class "column" ]
        [ input
            [ class "input has-shadow-only"
            , placeholder "Add item"
            , CreateItemClicked list |> onClick
            , CreateItemClicked list |> onFocus
            ]
            []
        ]


viewClearCheckedItemsButton : ShoppingList -> Html Msg
viewClearCheckedItemsButton list =
    div [ class "column has-gap-below" ]
        [ button
            [ class "button is-light is-pulled-right"
            , onClick <| ClearCheckedItems list
            ]
            [ text "clear checked items" ]
        ]


viewDeleteListModal : Model -> Html Msg
viewDeleteListModal model =
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
                    [ class "button is-primary is-large"
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
