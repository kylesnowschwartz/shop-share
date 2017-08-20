module Event exposing (..)

import Action exposing (..)
import Types exposing (..)
import UuidHelpers exposing (listId, itemId)


handleEvent : Model -> Event -> ( Model, Cmd Msg )
handleEvent model event =
    let
        sortModel lists =
            orderListsAndTheirItems model lists ! []

        fetchInitialLists id =
            { model | clientId = Just id }
                ! [ publishAction GetLists ]
    in
        case event of
            Registered id ->
                fetchInitialLists id

            GotLists lists ->
                sortModel lists

            CreatedList lists ->
                sortModel lists

            DeletedList lists ->
                sortModel lists

            UpdatedListTitle lists ->
                sortModel lists

            CreatedItem lists ->
                sortModel lists

            UpdatedItemText lists ->
                sortModel lists


orderListsAndTheirItems : Model -> List ShoppingList -> Model
orderListsAndTheirItems model lists =
    { model
        | shoppingLists =
            ((List.sortBy listId) << (List.map sortItemsById)) lists
    }


sortItemsById : ShoppingList -> ShoppingList
sortItemsById list =
    { list | listItems = List.sortBy itemId list.listItems }
