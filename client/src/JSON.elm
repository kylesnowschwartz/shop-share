module JSON exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Types exposing (..)
import Uuid exposing (Uuid)


-- DECODING


decodeEvent : String -> Result String Event
decodeEvent message =
    decodeString eventDecoder message


eventDecoder : Decoder Event
eventDecoder =
    (at [ "confirmAction", "type" ] string)
        |> andThen
            (\type_ ->
                case type_ of
                    "Register" ->
                        decodeRegisteredEvent

                    "GetLists" ->
                        decodeGotListsEvent

                    "CreateList" ->
                        decodeCreatedListEvent

                    "UpdateListTitle" ->
                        decodeUpdatedListTitleEvent

                    "DeleteList" ->
                        decodeDeletedListEvent

                    "CreateItem" ->
                        decodeCreatedItemEvent

                    "UpdateItemText" ->
                        decodeUpdatedItemTextEvent

                    other ->
                        fail <| "Unknown action received from server: " ++ other
            )


decodeRegisteredEvent : Decoder Event
decodeRegisteredEvent =
    map Registered
        (at [ "confirmAction", "data", "clientId" ] (map ClientId uuid))


decodeGotListsEvent : Decoder Event
decodeGotListsEvent =
    map GotLists
        (at [ "confirmAction", "data", "lists" ] (list decodeShoppingList))


decodeCreatedListEvent : Decoder Event
decodeCreatedListEvent =
    map CreatedList
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeDeletedListEvent : Decoder Event
decodeDeletedListEvent =
    map DeletedList
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeUpdatedListTitleEvent : Decoder Event
decodeUpdatedListTitleEvent =
    map UpdatedListTitle
        (at [ "confirmAction", "data", "list" ] (decodeShoppingList))


decodeCreatedItemEvent : Decoder Event
decodeCreatedItemEvent =
    map CreatedItem
        (at [ "confirmAction", "data", "item" ] (decodeItem))


decodeUpdatedItemTextEvent : Decoder Event
decodeUpdatedItemTextEvent =
    map UpdatedItemText
        (at [ "confirmAction", "data", "item" ] (decodeItem))


decodeShoppingList : Decoder ShoppingList
decodeShoppingList =
    decode ShoppingList
        |> required "listId" (map ShoppingListId uuid)
        |> required "title" string
        |> required "items" (list decodeItem)


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "itemId" (map ItemId uuid)
        |> required "text" string
        |> required "completed" bool


uuid : Decoder Uuid
uuid =
    string
        |> andThen
            (\str ->
                case (Uuid.fromString str) of
                    Just uuid_ ->
                        succeed uuid_

                    Nothing ->
                        fail str
            )


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


addItemAction : ShoppingListId -> String
addItemAction listId =
    "{\"action\": {\"type\": \"CreateItem\", \"text\": \"\", \"listId\": " ++ toString listId ++ "}}"


editItemAction : ShoppingListId -> ItemId -> String -> String
editItemAction listId itemId newName =
    "{\"action\": {\"type\": \"UpdateItemText\", \"text\": \"" ++ newName ++ "\", \"itemId\": " ++ toString itemId ++ ",\"listId\": " ++ toString listId ++ "}}"


deleteListAction : ShoppingList -> String
deleteListAction list =
    "{\"action\": {\"type\": \"DeleteList\", \"listId\": " ++ toString list.id ++ "}}"


editListTitleAction : ShoppingListId -> String -> String
editListTitleAction listId newTitle =
    "{\"action\": {\"type\": \"UpdateListTitle\", \"title\": \"" ++ newTitle ++ "\", \"listId\": " ++ toString listId ++ "}}"
