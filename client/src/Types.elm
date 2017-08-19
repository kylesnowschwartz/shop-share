module Types exposing (..)

import Uuid as Uuid exposing (Uuid)
import Random.Pcg as Pcg


-- TODO: Make shoppingLists & listItems Sets instead of Lists


type Msg
    = Register
    | GetLists
    | CreateNewList
    | DeleteList ShoppingList
    | ShoppingListTitleEdited ShoppingList String
    | ItemAdded ShoppingList
    | ItemTextEdited ShoppingList Item String
    | ItemChecked ShoppingList Item Bool
    | ItemDeleted ShoppingList Item
    | ClearCheckedItems ShoppingList
    | MessageReceived String


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : Maybe ClientId
    , errorMessage : Maybe String
    , uuidSeed : Pcg.Seed
    }


type alias ShoppingList =
    { id : ShoppingListId
    , title : String
    , listItems : List Item
    }


type alias Item =
    { id : ItemId
    , text : String
    , completed : Bool
    }


type Event
    = Registered ClientId
    | GotLists (List ShoppingList)
    | CreatedList (List ShoppingList)
    | DeletedList (List ShoppingList)
    | UpdatedListTitle (List ShoppingList)
    | CreatedItem (List ShoppingList)
    | UpdatedItemText (List ShoppingList)


type ShoppingListId
    = ShoppingListId Uuid


type ItemId
    = ItemId Uuid


type ClientId
    = ClientId Uuid
