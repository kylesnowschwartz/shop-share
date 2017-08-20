module UpdateHelpers exposing (..)

import Types exposing (..)
import List.Extra exposing (..)
import UuidHelpers exposing (..)


replaceList : ShoppingList -> Model -> Model
replaceList updatedList model =
    { model
        | shoppingLists = List.map (replaceIfUpdated updatedList) model.shoppingLists
    }


replaceIfUpdated : { a | id : b } -> { a | id : b } -> { a | id : b }
replaceIfUpdated updated a =
    if a.id == updated.id then
        updated
    else
        a


editListTitle : Model -> ShoppingList -> String -> ( Model, ShoppingList )
editListTitle model list newTitle =
    let
        updatedList =
            { list | title = newTitle }
    in
        ( replaceList updatedList model, updatedList )


deleteList : Model -> ShoppingList -> Model
deleteList model list =
    { model | shoppingLists = List.Extra.remove list model.shoppingLists }


updateItem : Item -> ShoppingList -> ShoppingList
updateItem updatedItem list =
    { list | listItems = List.map (replaceIfUpdated updatedItem) list.listItems }


deleteItem : Item -> ShoppingList -> ShoppingList
deleteItem item list =
    { list | listItems = remove item list.listItems }


listForItem : Model -> Item -> Maybe ShoppingList
listForItem model item =
    find (\l -> l.id == item.listId) model.shoppingLists


createListWithNewUuid : Model -> ( Model, ShoppingList )
createListWithNewUuid model =
    let
        ( newModel, newUuid ) =
            stepUuid model

        newList =
            { id = ListId newUuid, title = "", listItems = [] }
    in
        ( { newModel | shoppingLists = model.shoppingLists ++ [ newList ] }, newList )


createItemWithNewUuid : Model -> ShoppingList -> ( Model, Item )
createItemWithNewUuid model list =
    let
        ( modelWithNewSeed, newUuid ) =
            stepUuid model

        newItem =
            { id = ItemId newUuid, listId = list.id, text = "", completed = False }

        updatedList =
            { list | listItems = list.listItems ++ [ newItem ] }
    in
        ( replaceList updatedList modelWithNewSeed, newItem )
