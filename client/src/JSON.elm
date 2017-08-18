module JSON exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Types exposing (..)


-- DECODING


decodeAction : String -> Result String Action
decodeAction message =
    decodeString actionDecoder message


actionDecoder : Decoder Action
actionDecoder =
    (at [ "confirmAction", "type" ] string)
        |> andThen
            (\type_ ->
                case type_ of
                    "Register" ->
                        decodeRegister

                    "GetLists" ->
                        decodeGetLists

                    "CreateList" ->
                        decodeCreateList

                    "UpdateListTitle" ->
                        decodeEditListTitle

                    "DeleteList" ->
                        decodeDeleteList

                    "CreateItem" ->
                        decodeAddItem

                    "UpdateItemText" ->
                        decodeUpdateItem

                    other ->
                        fail <| "Unknown action received from server: " ++ other
            )


decodeRegister : Decoder Action
decodeRegister =
    map Register
        (at [ "confirmAction", "data", "clientId" ] int)


decodeGetLists : Decoder Action
decodeGetLists =
    map GetLists
        (at [ "confirmAction", "data", "lists" ] (list decodeShoppingList))


decodeCreateList : Decoder Action
decodeCreateList =
    map CreateList
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeDeleteList : Decoder Action
decodeDeleteList =
    map DeleteShoppingList
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeEditListTitle : Decoder Action
decodeEditListTitle =
    map EditListTitle
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeAddItem : Decoder Action
decodeAddItem =
    map AddListItem
        (at [ "confirmAction", "data", "item" ] (decodeListItem))


decodeUpdateItem : Decoder Action
decodeUpdateItem =
    map UpdateItemText
        (at [ "confirmAction", "data", "item" ] (decodeListItem))


decodeShoppingList : Decoder ShoppingList
decodeShoppingList =
    decode ShoppingList
        |> required "listId" int
        |> required "title" string
        |> required "items" (list decodeListItem)


decodeListItem : Decoder ListItem
decodeListItem =
    decode ListItem
        |> required "itemId" int
        |> required "text" string
        |> required "completed" bool


registerSample : String
registerSample =
    "{\"confirmAction\": {\"type\": \"Register\", \"data\": { \"clientId\": 1 }}}"


getListsSample : String
getListsSample =
    "{\"confirmAction\": {\"type\": \"GetLists\", \"data\": { \"lists\": [ { \"listId\": 1, \"title\": \"Test list\", \"items\": [ { \"itemId\": 1, \"text\": \"Get some Haskell down ya\", \"completed\": false}]} ] }}}"


invalidActionSample : String
invalidActionSample =
    "{\"confirmAction\": {\"type\": \"PoosAndWeesAndBucketsOfCheese\", \"data\": { \"clientId\": 1 }}}"



-- ENCODING


registerAction : String
registerAction =
    "{\"action\": {\"type\": \"Register\"}}"


getListsAction : String
getListsAction =
    "{\"action\": {\"type\": \"GetLists\"}}"


createListAction : String
createListAction =
    "{\"action\": {\"type\": \"CreateList\", \"title\": \"\"}}"


addListItemAction : ShoppingListId -> String
addListItemAction listId =
    "{\"action\": {\"type\": \"CreateItem\", \"text\": \"\", \"listId\": " ++ toString listId ++ "}}"


editListItemAction : ShoppingListId -> ItemId -> String -> String
editListItemAction listId itemId newName =
    "{\"action\": {\"type\": \"UpdateItemText\", \"text\": \"" ++ newName ++ "\", \"itemId\": " ++ toString itemId ++ ",\"listId\": " ++ toString listId ++ "}}"


deleteListAction : ShoppingList -> String
deleteListAction list =
    "{\"action\": {\"type\": \"DeleteList\", \"listId\": " ++ toString list.id ++ "}}"


editListTitleAction : ShoppingListId -> String -> String
editListTitleAction listId newTitle =
    "{\"action\": {\"type\": \"UpdateListTitle\", \"title\": \"" ++ newTitle ++ "\", \"listId\": " ++ toString listId ++ "}}"
