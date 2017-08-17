module JSON exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Types exposing (..)


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

                    other ->
                        fail <| "Unknown action received from server: " ++ other
            )


decodeRegister : Decoder Action
decodeRegister =
    map Register (at [ "confirmAction", "data", "clientId" ] int)


decodeGetLists : Decoder Action
decodeGetLists =
    map GetLists
        (at [ "confirmAction", "data", "lists" ] (list decodeShoppingList))


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
