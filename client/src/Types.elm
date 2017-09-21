module Types exposing (..)

import Uuid as Uuid exposing (Uuid)
import Random.Pcg as Pcg
import Date exposing (Date)


-- A Msg is sent to us by the Elm Runtime when something happens.


type Msg
    = CreateListClicked
    | DeleteListClicked ShoppingList
    | ListTitleEdited ShoppingList String
    | CreateItemClicked ShoppingList
    | ItemTextEdited ShoppingList Item String
    | ItemChecked ShoppingList Item Bool
    | DeleteItemClicked ShoppingList Item
    | ClearCheckedItems ShoppingList
    | DeleteListCancelClicked
    | DeleteListConfirmClicked
    | WSMessageReceived String



-- An Action is something we want to perform on the server.


type Action
    = Register
    | GetLists
    | CreateList ShoppingList
    | DeleteList ShoppingList
    | UpdateList ShoppingList
    | CreateItem Item
    | UpdateItem Item
    | DeleteItem Item



-- An Event is something that happened on the server.


type Event
    = Registered ClientId
    | GotLists (List ShoppingList)
    | CreatedList ShoppingList
    | DeletedList
    | UpdatedListTitle ShoppingList
    | CreatedItem Item
    | UpdatedItemText Item
    | DeletedItem


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : Maybe ClientId
    , errorMessage : Maybe String
    , uuidSeed : Pcg.Seed
    , listToDelete : Maybe ShoppingList
    }


type alias ShoppingList =
    { id : ListId
    , title : String
    , listItems : List Item
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


type alias Item =
    { id : ItemId
    , text : String
    , completed : Bool
    , listId : ListId
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


type ListId
    = ListId Uuid


type ItemId
    = ItemId Uuid


type ClientId
    = ClientId Uuid
