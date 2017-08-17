module ShopShare exposing (Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick)
import List.Extra exposing (..)
import WebSocket as WS
import Types exposing (..)
import JSON


-- MODEL


init : ( Model, Cmd Msg )
init =
    { shoppingLists =
        [ { id = 1
          , name = "Groceries"
          , listItems =
                [ { id = 1
                  , text = "Apples"
                  , completed = False
                  }
                ]
          }
        ]
    , clientId = 0
    }
        ! []



-- UPDATE


type Msg
    = ShoppingListNameEdited Int String
    | ItemAdded Int String
    | ItemEdited Int Int String
    | ItemChecked Int Int Bool
    | ClearCheckedItems Int
    | MessageReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShoppingListNameEdited updatedListId newName ->
            { model | shoppingLists = updateShoppingList model updatedListId (updateName newName) } ! []

        ItemAdded updatedListId newItem ->
            { model | shoppingLists = updateShoppingList model updatedListId (addItem newItem) } ! []

        ItemEdited updatedListId newItemId newItemText ->
            { model | shoppingLists = updateShoppingList model updatedListId (editItem newItemText newItemId) } ! []

        ItemChecked updatedListId newItemId itemChecked ->
            { model | shoppingLists = updateShoppingList model updatedListId (checkItem itemChecked newItemId) } ! []

        ClearCheckedItems updatedListId ->
            { model | shoppingLists = updateShoppingList model updatedListId (clearCheckedItems) } ! []

        MessageReceived message ->
            handleMessage model message ! []


handleMessage : Model -> String -> Model
handleMessage model message =
    case JSON.decodeAction message of
        Ok action ->
            case action of
                Register newId ->
                    { model | clientId = newId }

                GetLists lists ->
                    { model | shoppingLists = lists }

                CreateList list ->
                    model

        -- TODO: Really need to return all the lists from CreateList
        Err err ->
            model


clearCheckedItems : ShoppingList -> ShoppingList
clearCheckedItems list =
    { list | listItems = List.Extra.filterNot .completed list.listItems }


updateName : String -> ShoppingList -> ShoppingList
updateName newName list =
    { list | name = newName }


addItem : String -> ShoppingList -> ShoppingList
addItem newItem list =
    { list | listItems = list.listItems ++ [ { id = (incrementItemId list), text = newItem, completed = False } ] }


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


incrementItemId : ShoppingList -> ItemId
incrementItemId list =
    Maybe.withDefault 0 (List.maximum (List.map .id list.listItems)) + 1


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
    div []
        [ h1 [] [ text "Hello shop share!" ]
        , ol [ id "lists" ] (List.map viewShoppingList model.shoppingLists)
        ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    li []
        [ input
            [ placeholder "List name"
            , onInput (ShoppingListNameEdited list.id)
            , value list.name
            ]
            []
        , div []
            [ ul [ class "list" ]
                (List.concat [ List.map (viewListItem list.id) list.listItems, [ viewAddListItem list ] ])
            ]
        , div []
            [ viewClearCheckedItems list ]
        ]


viewListItem : ShoppingListId -> ListItem -> Html Msg
viewListItem listId item =
    li []
        [ input [ placeholder "Item name", value item.text, onInput (ItemEdited listId item.id) ] []
        , label [] [ input [ type_ "checkbox", checked item.completed, Html.Events.onCheck (ItemChecked listId item.id) ] [] ]
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    li []
        [ input
            [ placeholder "Add a new list item"
            , onInput (ItemAdded list.id)
            ]
            []
        ]


viewClearCheckedItems : ShoppingList -> Html Msg
viewClearCheckedItems list =
    button [ onClick (ClearCheckedItems list.id) ] [ text "clear checked items" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen "ws://89f26dac.ngrok.io" MessageReceived
