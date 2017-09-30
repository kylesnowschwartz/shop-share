module UpdateHelpers
    exposing
        ( createItemWithNewUuid
        , createListWithNewUuid
        , deleteItem
        , deleteItems
        , deleteList
        , replaceItem
        , replaceItem_
        , replaceList
        , replaceList_
        )

import Types exposing (..)
import List.Extra exposing (replaceIf, remove, find)
import UuidHelpers exposing (..)
import RecordHelpers exposing (replaceById)
import List.Extra exposing (replaceIf, filterNot)


replaceList : Model -> ShoppingList -> Model
replaceList model updatedList =
    { model | lists = replaceById updatedList model.lists }


replaceList_ : Model -> ShoppingList -> ( Model, ShoppingList )
replaceList_ model list =
    ( replaceList model list, list )


deleteList : Model -> ShoppingList -> Model
deleteList model list =
    { model | lists = remove list model.lists, listToDelete = Nothing }


replaceItem : Model -> Item -> Model
replaceItem model item =
    { model | lists = List.map (replaceItemInList item) model.lists }


replaceItem_ : Model -> Item -> ( Model, Item )
replaceItem_ model item =
    ( replaceItem model item, item )


replaceItemInList : Item -> ShoppingList -> ShoppingList
replaceItemInList item list =
    { list | listItems = replaceById item list.listItems }


editItemText : Model -> Item -> ( Model, Item )
editItemText model item =
    ( replaceItem model item, item )


checkItem : Model -> Item -> ( Model, Item )
checkItem model item =
    ( replaceItem model item, item )


deleteItems : Model -> (Item -> Bool) -> ShoppingList -> Model
deleteItems model removalPredicate list =
    { list | listItems = filterNot removalPredicate list.listItems }
        |> replaceList model


deleteItem : Model -> Item -> Model
deleteItem model item =
    { model | lists = List.map (deleteItemInList item) model.lists }


deleteItemInList : Item -> ShoppingList -> ShoppingList
deleteItemInList item list =
    { list | listItems = remove item list.listItems }


createListWithNewUuid : Model -> ( Model, ShoppingList )
createListWithNewUuid model =
    let
        ( modelWithNewSeed, newUuid ) =
            stepUuid model

        newList =
            { id = ListId newUuid
            , title = ""
            , listItems = []
            , createdAt = Nothing
            , updatedAt = Nothing
            }
    in
        ( { modelWithNewSeed | lists = model.lists ++ [ newList ] }, newList )


createItemWithNewUuid : Model -> ShoppingList -> ( Model, Item )
createItemWithNewUuid model list =
    let
        ( modelWithNewSeed, newUuid ) =
            stepUuid model

        newItem =
            { id = ItemId newUuid
            , listId = list.id
            , text = ""
            , completed = False
            , createdAt = Nothing
            , updatedAt = Nothing
            }

        updatedList =
            { list | listItems = list.listItems ++ [ newItem ] }
    in
        ( replaceList modelWithNewSeed updatedList, newItem )
