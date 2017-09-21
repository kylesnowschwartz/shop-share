module UpdateHelpers exposing (..)

import Date
import Types exposing (..)
import List.Extra exposing (..)
import UuidHelpers exposing (..)


replaceLists : List ShoppingList -> Model -> Model
replaceLists newLists model =
    { model | shoppingLists = sortListsAndItems newLists }


addList : ShoppingList -> Model -> Model
addList newList model =
    { model
        | shoppingLists =
            sortListsAndItems <|
                List.map (replaceIfUpdated newList) model.shoppingLists
    }


replaceList : ShoppingList -> Model -> Model
replaceList updatedList model =
    { model
        | shoppingLists =
            sortListsAndItems <|
                List.map (replaceIfUpdated updatedList) model.shoppingLists
    }


addItem : Item -> Model -> Model
addItem newItem model =
    let
        newLists =
            sortListsAndItems <| List.map updateList model.shoppingLists

        updateList list =
            { list | listItems = List.map (replaceIfUpdated newItem) list.listItems }
    in
        { model | shoppingLists = newLists }


replaceItem : Item -> Model -> Model
replaceItem updatedItem model =
    let
        newLists =
            sortListsAndItems <| List.map updateList model.shoppingLists

        updateList list =
            { list | listItems = List.map (replaceIfUpdated updatedItem) list.listItems }
    in
        { model | shoppingLists = newLists }


sortListsAndItems : List ShoppingList -> List ShoppingList
sortListsAndItems lists =
    lists |> earliestFirst << List.map sortItems


sortItems : ShoppingList -> ShoppingList
sortItems list =
    { list | listItems = earliestFirst list.listItems }


replaceIfUpdated : { a | id : b } -> { a | id : b } -> { a | id : b }
replaceIfUpdated updated a =
    if a.id == updated.id then
        updated
    else
        a


earliestFirst : List { a | createdAt : Maybe b } -> List { a | createdAt : Maybe b }
earliestFirst a =
    let
        earliestFirstDefaultingToLast a =
            case a.createdAt of
                Nothing ->
                    toString (Date.fromTime 2147483647.0)

                Just date ->
                    toString date
    in
        (List.sortBy earliestFirstDefaultingToLast a)


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
            { id = ListId newUuid
            , title = ""
            , listItems = []
            , createdAt = Nothing
            , updatedAt = Nothing
            }
    in
        ( { newModel | shoppingLists = model.shoppingLists ++ [ newList ] }, newList )


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
