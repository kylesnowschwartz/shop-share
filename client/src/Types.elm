module Types exposing (..)


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : ClientId
    }


type alias ShoppingList =
    { id : ShoppingListId
    , name : String
    , listItems : List ListItem
    }


type alias ListItem =
    { id : ItemId
    , text : String
    , completed : Bool
    }


type Action
    = Register ClientId
    | GetLists (List ShoppingList)
    | CreateList ShoppingList


type alias ShoppingListId =
    Int


type alias ItemId =
    Int


type alias ClientId =
    Int
