module Types exposing (..)


type alias Model =
    { shoppingLists : List ShoppingList
    , clientId : Maybe ClientId
    , errorMessage : Maybe String
    }


type alias ShoppingList =
    { id : ShoppingListId
    , title : String
    , listItems : List ListItem
    }


type alias ListItem =
    { id : ItemId
    , text : String
    , completed : Bool
    }


type alias Group =
    { id : GroupId }


type Action
    = Register ClientId
    | GetLists (List ShoppingList)
    | CreateList ShoppingList
    | DeleteShoppingList ShoppingList
    | EditListTitle ShoppingList
    | AddListItem ListItem


type alias ShoppingListId =
    Int


type alias ItemId =
    Int


type alias ClientId =
    Int


type alias GroupId =
    Int
