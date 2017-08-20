module Types exposing (..)

-- TODO: Make shoppingLists & listItems Sets instead of Lists

import Uuid as Uuid exposing (Uuid)
import Random.Pcg as Pcg


type Msg
    = CreateListClicked
    | DeleteListClicked ShoppingList
    | ListTitleEdited ShoppingList String
    | CreateItemClicked ShoppingList
    | ItemTextEdited ShoppingList Item String
    | ItemChecked ShoppingList Item Bool
    | DeleteItemClicked ShoppingList Item
    | ClearCheckedItems ShoppingList
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
    | CreatedList (List ShoppingList)
    | DeletedList (List ShoppingList)
    | UpdatedListTitle (List ShoppingList)
    | CreatedItem (List ShoppingList)
    | UpdatedItemText (List ShoppingList)


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : Maybe ClientId
    , errorMessage : Maybe String
    , uuidSeed : Pcg.Seed
    }


type alias ShoppingList =
    { id : ListId
    , title : String
    , listItems : List Item
    }


type alias Item =
    { id : ItemId
    , text : String
    , completed : Bool
    , listId : ListId
    }


type ListId
    = ListId Uuid


type ItemId
    = ItemId Uuid


type ClientId
    = ClientId Uuid
