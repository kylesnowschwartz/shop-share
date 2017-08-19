module JSON exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (..)
import Types exposing (..)
import Uuid exposing (Uuid)
import UuidHelpers exposing (..)


-- DECODING


decodeEvent : String -> Result String Event
decodeEvent message =
    decodeString eventDecoder message


eventDecoder : Decoder Event
eventDecoder =
    (at [ "confirmAction", "type" ] Decode.string)
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
        (at [ "confirmAction", "data", "lists" ] (Decode.list decodeShoppingList))


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
        |> required "title" Decode.string
        |> required "items" (Decode.list decodeItem)


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "itemId" (map ItemId uuid)
        |> required "text" Decode.string
        |> required "completed" Decode.bool


uuid : Decoder Uuid
uuid =
    Decode.string
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


encodeMsg : Msg -> Encode.Value -> String
encodeMsg msg msgData =
    encode 0
        (object
            [ ( "action"
              , object
                    [ ( "type", Encode.string (msgToActionType msg) )
                    , ( "data", msgData )
                    ]
              )
            ]
        )


encodeList : ShoppingList -> Encode.Value
encodeList list =
    object
        [ ( "listId", Encode.string (listIdToString list) )
        , ( "title", Encode.string list.title )
        ]


encodeItem : Item -> ShoppingList -> Encode.Value
encodeItem item list =
    object
        [ ( "itemId", Encode.string (itemIdToString item) )
        , ( "listId", Encode.string (listIdToString list) )
        , ( "text", Encode.string item.text )
        ]


emptyObject : Encode.Value
emptyObject =
    object []


msgToActionType : Msg -> String
msgToActionType msg =
    case msg of
        Register ->
            "Register"

        GetLists ->
            "GetLists"

        CreateNewList ->
            "CreateNewList"

        DeleteList _ ->
            "DeleteList"

        ShoppingListTitleEdited _ _ ->
            "ShoppingListTitleEdited"

        ItemAdded _ ->
            "CreateItem"

        ItemTextEdited _ _ _ ->
            "UpdateItemText"

        ItemChecked _ _ _ ->
            "CompleteItem"

        ItemDeleted _ _ ->
            "DeleteItem"

        ClearCheckedItems _ ->
            "ClearCheckedItems"

        MessageReceived _ ->
            "MessageReceived"



-- registerAction : String
-- registerAction =
--     "{\"action\": {\"type\": \"Register\"}}"
-- getListsAction : String
-- getListsAction =
--     "{\"action\": {\"type\": \"GetLists\"}}"
-- createListAction : String
-- createListAction =
--     "{\"action\": {\"type\": \"CreateList\", \"title\": \"\"}}"
-- addItemAction : ShoppingListId -> String
-- addItemAction listId =
--     "{\"action\": {\"type\": \"CreateItem\", \"text\": \"\", \"listId\": " ++ toString listId ++ "}}"
-- editItemAction : ShoppingListId -> ItemId -> String -> String
-- editItemAction listId itemId newName =
--     "{\"action\": {\"type\": \"UpdateItemText\", \"text\": \"" ++ newName ++ "\", \"itemId\": " ++ toString itemId ++ ",\"listId\": " ++ toString listId ++ "}}"
-- deleteListAction : ShoppingList -> String
-- deleteListAction list =
--     "{\"action\": {\"type\": \"DeleteList\", \"listId\": " ++ toString list.id ++ "}}"
-- editListTitleAction : ShoppingListId -> String -> String
-- editListTitleAction listId newTitle =
--     "{\"action\": {\"type\": \"UpdateListTitle\", \"title\": \"" ++ newTitle ++ "\", \"listId\": " ++ toString listId ++ "}}"
