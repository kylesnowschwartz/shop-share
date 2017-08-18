module ShopShare exposing (Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import List.Extra exposing (..)
import WebSocket as WS
import Types exposing (..)
import JSON


-- MODEL


wsAddress : String
wsAddress =
    "ws://afebe343.ngrok.io"


init : ( Model, Cmd Msg )
init =
    { shoppingLists =
        [ { id = 1
          , title = "Groceries"
          , listItems =
                [ { id = 1
                  , text = "Apples"
                  , completed = False
                  }
                ]
          }
        ]
    , clientId = Nothing
    , errorMessage = Nothing
    }
        ! [ WS.send wsAddress (JSON.registerAction) ]



-- UPDATE


type Msg
    = ShoppingListTitleEdited Int String
    | CreateNewList
    | DeleteList ShoppingList
    | ItemAdded ShoppingListId
    | ItemEdited ShoppingListId ItemId String
    | ItemChecked ShoppingListId ItemId Bool
    | ItemDeleted ShoppingListId ListItem
    | ClearCheckedItems ShoppingListId
    | MessageReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewList ->
            { model | shoppingLists = addList model.shoppingLists } ! [ WS.send wsAddress JSON.createListAction ]

        DeleteList list ->
            { model | shoppingLists = List.Extra.remove list model.shoppingLists } ! [ WS.send wsAddress (JSON.deleteListAction list) ]

        ShoppingListTitleEdited updatedListId newTitle ->
            { model | shoppingLists = updateShoppingList model updatedListId (updateTitle newTitle) } ! [ WS.send wsAddress (JSON.editListTitleAction updatedListId newTitle) ]

        ItemAdded updatedListId ->
            { model | shoppingLists = updateShoppingList model updatedListId (addItem "") } ! [ WS.send wsAddress (JSON.addListItemAction updatedListId) ]

        ItemEdited updatedListId newItemId newItemText ->
            { model | shoppingLists = updateShoppingList model updatedListId (editItem newItemText newItemId) } ! []

        ItemChecked updatedListId newItemId itemChecked ->
            { model | shoppingLists = updateShoppingList model updatedListId (checkItem itemChecked newItemId) } ! []

        ItemDeleted updatedListId deletedItem ->
            { model | shoppingLists = updateShoppingList model updatedListId (deleteItem deletedItem) } ! []

        ClearCheckedItems updatedListId ->
            { model | shoppingLists = updateShoppingList model updatedListId (clearCheckedItems) } ! []

        MessageReceived message ->
            handleMessage model message


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    case JSON.decodeAction message of
        Ok action ->
            case action of
                Register newId ->
                    { model | clientId = Just newId } ! [ WS.send wsAddress JSON.getListsAction ]

                GetLists lists ->
                    { model | shoppingLists = (Debug.log "lists: " lists) } ! []

                CreateList _ ->
                    model ! []

                DeleteShoppingList _ ->
                    model ! []

                EditListTitle _ ->
                    model ! []

                AddListItem _ ->
                    model ! []

        -- TODO: Really need to return all the lists from CreateList
        Err err ->
            { model | errorMessage = Just err } ! []


clearCheckedItems : ShoppingList -> ShoppingList
clearCheckedItems list =
    { list | listItems = List.Extra.filterNot .completed list.listItems }


updateTitle : String -> ShoppingList -> ShoppingList
updateTitle newTitle list =
    { list | title = newTitle }


addItem : String -> ShoppingList -> ShoppingList
addItem newItem list =
    { list | listItems = list.listItems ++ [ { id = (incrementItemId list), text = newItem, completed = False } ] }


addList : List ShoppingList -> List ShoppingList
addList lists =
    lists ++ [ { id = (incrementListId lists), title = "", listItems = [] } ]


editItem : String -> ItemId -> ShoppingList -> ShoppingList
editItem newItemText newItemId list =
    let
        applyIfEdited item =
            if item.id == newItemId then
                { item | text = newItemText }
            else
                item
    in
        { list | listItems = List.map applyIfEdited list.listItems }


checkItem : Bool -> Int -> ShoppingList -> ShoppingList
checkItem itemChecked newItemId list =
    let
        applyIfChecked item =
            if item.id == newItemId then
                { item | completed = itemChecked }
            else
                item
    in
        { list | listItems = List.map applyIfChecked list.listItems }


deleteItem : ListItem -> ShoppingList -> ShoppingList
deleteItem deletedItem list =
    { list | listItems = List.Extra.remove deletedItem list.listItems }


incrementItemId : ShoppingList -> ItemId
incrementItemId list =
    Maybe.withDefault 0 (List.maximum (List.map .id list.listItems)) + 1


incrementListId : List ShoppingList -> ShoppingListId
incrementListId lists =
    Maybe.withDefault 0 (List.maximum (List.map .id lists)) + 1


updateShoppingList : Model -> ShoppingListId -> (ShoppingList -> ShoppingList) -> List ShoppingList
updateShoppingList model updatedId updateFunction =
    let
        filteredUpdate list =
            if list.id == updatedId then
                updateFunction list
            else
                list
    in
        List.map filteredUpdate model.shoppingLists



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ h1 [] [ text "Hello shop share!" ]
        , dl [ id "container lists" ] (List.map viewShoppingList model.shoppingLists)
        , div [ class "section" ]
            [ a [ onClick (CreateNewList) ] [ text "make a new list" ]
            ]
        , viewErrors model
        , viewClientId model
        ]


viewErrors : Model -> Html Msg
viewErrors model =
    div [] [ text (Maybe.withDefault "" model.errorMessage) ]


viewClientId : Model -> Html Msg
viewClientId model =
    case model.clientId of
        Nothing ->
            div [] []

        Just id ->
            h3 [] [ text ("Registered with server as client " ++ toString id) ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    dd [ class "content" ]
        [ input
            [ placeholder "List title"
            , onInput (ShoppingListTitleEdited list.id)
            , value list.title
            ]
            []
        , a [ tabindex -1, class "delete is-small", onClick (DeleteList list) ] []
        , div []
            [ dl [ class "list" ]
                (List.concat [ List.map (viewListItem list.id) list.listItems, [ viewAddListItem list ] ])
            ]
        , div []
            [ viewClearCheckedItems list ]
        ]


viewListItem : ShoppingListId -> ListItem -> Html Msg
viewListItem listId item =
    dd []
        [ input [ tabindex 2, placeholder "Item name", value item.text, onInput (ItemEdited listId item.id) ] []
        , label [ class "checkbox" ]
            [ input [ type_ "checkbox", checked item.completed, Html.Events.onCheck (ItemChecked listId item.id) ] [] ]
        , a [ tabindex -1, class "delete is-small", onClick (ItemDeleted listId item) ] []
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    dd []
        [ input
            [ placeholder "Add a new list item"
            , onClick (ItemAdded list.id)
            ]
            []
        ]


viewClearCheckedItems : ShoppingList -> Html Msg
viewClearCheckedItems list =
    button [ class "button is-primary", onClick (ClearCheckedItems list.id) ] [ text "clear checked items" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen wsAddress MessageReceived
