module UpdateHelpers exposing (..)

import Types exposing (..)
import List.Extra exposing (..)
import UuidHelpers exposing (..)


listForItem : Model -> Item -> Maybe ShoppingList
listForItem model item =
    find (\l -> l.id == item.listId) model.shoppingLists


updateItem : Item -> Maybe ShoppingList -> Maybe ShoppingList
updateItem updatedItem list =
    let
        f l =
            { l | listItems = List.map (replaceIfUpdated updatedItem) l.listItems }
    in
        Maybe.map f list


deleteItem : Item -> Maybe ShoppingList -> Maybe ShoppingList
deleteItem item list =
    Maybe.map (\l -> { l | listItems = remove item l.listItems }) list


updateShoppingList : ShoppingList -> Model -> Model
updateShoppingList updatedList model =
    { model | shoppingLists = List.map (replaceIfUpdated updatedList) model.shoppingLists }


replaceIfUpdated : { a | id : b } -> { a | id : b } -> { a | id : b }
replaceIfUpdated updated a =
    if a.id == updated.id then
        updated
    else
        a


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
        ( updateShoppingList updatedList modelWithNewSeed, newItem )
