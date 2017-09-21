module Event exposing (..)

import Action exposing (..)
import Types exposing (..)
import UpdateHelpers exposing (earliestFirst)


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

            DeletedItem lists ->
                sortModel lists


orderListsAndTheirItems : Model -> List ShoppingList -> Model
orderListsAndTheirItems model lists =
    { model
        | shoppingLists =
            (earliestFirst << (List.map sortItems)) lists
    }


sortItems : ShoppingList -> ShoppingList
sortItems list =
    { list | listItems = earliestFirst list.listItems }
