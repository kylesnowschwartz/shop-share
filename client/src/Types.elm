module Types exposing (..)


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : Maybe ClientId
    , errorMessage : Maybe String
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


type alias Group =
    { id : GroupId }


type Event
    = Registered ClientId
    | GotLists (List ShoppingList)
    | CreatedList ShoppingList
    | DeletedList ShoppingList
    | UpdatedListTitle ShoppingList
    | CreatedItem Item
    | UpdatedItemText Item


type alias ShoppingListId =
    Int


type alias ItemId =
    Int


type alias ClientId =
    Int


type alias GroupId =
    Int
