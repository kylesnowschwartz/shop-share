module ShopShare exposing (Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import JSON
import List.Extra exposing (..)
import Types exposing (..)
import Uuid as Uuid exposing (Uuid)
import UuidHelpers exposing (..)
import WebSocket as WS


-- MODEL


wsAddress : String
wsAddress =
    "ws://localhost:8000"


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { shoppingLists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    }
        ! [ WS.send wsAddress (JSON.registerAction) ]



-- UPDATE


type Msg
    = ShoppingListTitleEdited ShoppingListId String
    | CreateNewList
    | DeleteList ShoppingList
    | ItemAdded ShoppingListId
    | ItemEdited ShoppingListId ItemId String
    | ItemChecked ShoppingListId ItemId Bool
    | ItemDeleted ShoppingListId Item
    | ClearCheckedItems ShoppingListId
    | MessageReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewList ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model
            in
                { modelWithNewSeed | shoppingLists = addList newUuid model.shoppingLists } ! [ WS.send wsAddress JSON.createListAction ]

        DeleteList list ->
            { model | shoppingLists = List.Extra.remove list model.shoppingLists } ! [ WS.send wsAddress (JSON.deleteListAction list) ]

        ShoppingListTitleEdited updatedListId newTitle ->
            { model | shoppingLists = updateShoppingList model updatedListId (updateTitle newTitle) } ! [ WS.send wsAddress (JSON.editListTitleAction updatedListId newTitle) ]

        ItemAdded updatedListId ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model
            in
                { modelWithNewSeed | shoppingLists = updateShoppingList model updatedListId (addItem newUuid "") } ! [ WS.send wsAddress (JSON.addItemAction updatedListId) ]

        ItemEdited updatedListId newItemId newItemText ->
            { model | shoppingLists = updateShoppingList model updatedListId (editItem newItemText newItemId) } ! [ WS.send wsAddress (JSON.editItemAction updatedListId newItemId newItemText) ]

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
    let
        fetchAllLists =
            model ! [ WS.send wsAddress JSON.getListsAction ]
    in
        case JSON.decodeEvent message of
            Ok action ->
                case action of
                    Registered newId ->
                        { model | clientId = Just newId } ! [ WS.send wsAddress JSON.getListsAction ]

                    GotLists lists ->
                        { model | shoppingLists = orderListsAndTheirItems lists } ! []

                    CreatedList newList ->
                        fetchAllLists

                    DeletedList _ ->
                        fetchAllLists

                    UpdatedListTitle _ ->
                        fetchAllLists

                    CreatedItem _ ->
                        fetchAllLists

                    UpdatedItemText _ ->
                        fetchAllLists

            Err err ->
                { model | errorMessage = Just err } ! []


orderListsAndTheirItems : List ShoppingList -> List ShoppingList
orderListsAndTheirItems =
    (List.sortBy comparableListId) << (List.map sortItems)


sortItems : ShoppingList -> ShoppingList
sortItems list =
    { list | listItems = List.sortBy comparableItemId list.listItems }


clearCheckedItems : ShoppingList -> ShoppingList
clearCheckedItems list =
    { list | listItems = List.Extra.filterNot .completed list.listItems }


updateTitle : String -> ShoppingList -> ShoppingList
updateTitle newTitle list =
    { list | title = newTitle }


addItem : Uuid -> String -> ShoppingList -> ShoppingList
addItem newUuid text list =
    { list | listItems = list.listItems ++ [ { id = ItemId newUuid, text = text, completed = False } ] }


addList : Uuid -> List ShoppingList -> List ShoppingList
addList newUuid lists =
    lists ++ [ { id = ShoppingListId newUuid, title = "", listItems = [] } ]


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


checkItem : Bool -> ItemId -> ShoppingList -> ShoppingList
checkItem itemChecked newItemId list =
    let
        applyIfChecked item =
            if item.id == newItemId then
                { item | completed = itemChecked }
            else
                item
    in
        { list | listItems = List.map applyIfChecked list.listItems }


deleteItem : Item -> ShoppingList -> ShoppingList
deleteItem deletedItem list =
    { list | listItems = List.Extra.remove deletedItem list.listItems }


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


viewListItem : ShoppingListId -> Item -> Html Msg
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
