module UpdateHelpers exposing (..)

-- import Date

import Types exposing (..)
import List.Extra exposing (replaceIf, remove, find)
import UuidHelpers exposing (..)


-- A weak point of the current version of Elm, in my opinion, is the
-- record update syntax. These helper functions aims to alleviate some
-- of that annoyance.

import List.Extra exposing (replaceIf)


{-| Replace an element in a list based on id. This is essentially just
teaching List.Extra.replaceIf about ids.
-}
replaceById : { a | id : b } -> List { a | id : b } -> List { a | id : b }
replaceById replacement =
    replaceIf (\x -> x.id == replacement.id) replacement


replaceList : ShoppingList -> Model -> Model
replaceList updatedList model =
    { model | lists = replaceById updatedList model.lists }


addItem : Item -> Model -> Model
addItem newItem model =
    let
        newLists =
            List.map updateList model.lists

        updateList list =
            { list | listItems = replaceById newItem list.listItems }
    in
        { model | lists = newLists }


replaceItem : Item -> Model -> Model
replaceItem updatedItem model =
    let
        newLists =
            List.map updateList model.lists

        updateList list =
            { list | listItems = replaceById updatedItem list.listItems }
    in
        { model | lists = newLists }


editListTitle : Model -> ShoppingList -> String -> ( Model, ShoppingList )
editListTitle model list newTitle =
    let
        updatedList =
            { list | title = newTitle }
    in
        ( replaceList updatedList model, updatedList )


deleteList : Model -> ShoppingList -> Model
deleteList model list =
    { model | lists = remove list model.lists }


updateItem : Item -> ShoppingList -> ShoppingList
updateItem updatedItem list =
    { list | listItems = replaceById updatedItem list.listItems }


deleteItem : Item -> ShoppingList -> ShoppingList
deleteItem item list =
    { list | listItems = remove item list.listItems }


listForItem : Model -> Item -> Maybe ShoppingList
listForItem model item =
    find (\l -> l.id == item.listId) model.lists


createListWithNewUuid : Model -> ( Model, ShoppingList )
createListWithNewUuid model =
    let
        ( newModel, newUuid ) =
            stepUuid model

        newList =
            { id = ListId newUuid
            , title = ""
            , listItems = []
            , createdAt = Nothing
            , updatedAt = Nothing
            }
    in
        ( { newModel | lists = model.lists ++ [ newList ] }, newList )


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
        ( replaceList updatedList modelWithNewSeed, newItem )
