module JSON exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (..)
import Types exposing (..)
import Uuid exposing (Uuid)
import UuidHelpers exposing (..)


-- ENCODING


encodeAction : Action -> String
encodeAction action =
    encode 0
        (object
            [ ( "action"
              , (encodeActionTypeAndData action)
              )
            ]
        )


encodeActionTypeAndData : Action -> Encode.Value
encodeActionTypeAndData action =
    let
        ( actionType, actionData ) =
            case action of
                Register ->
                    ( "Register", object [] )

                GetLists ->
                    ( "GetLists", object [] )

                CreateList list ->
                    ( "CreateList", encodeList list )

                DeleteList list ->
                    ( "DeleteList", encodeList list )

                UpdateList list ->
                    ( "UpdateList", encodeList list )

                CreateItem item ->
                    ( "CreateItem", encodeItem item )

                UpdateItem item ->
                    ( "UpdateItem", encodeItem item )

                DeleteItem item ->
                    ( "DeleteItem", encodeItem item )
    in
        object
            [ ( "type", Encode.string actionType )
            , ( "data", actionData )
            ]


encodeList : ShoppingList -> Encode.Value
encodeList list =
    object
        [ ( "listId", Encode.string (listId list) )
        , ( "title", Encode.string list.title )
        ]


encodeItem : Item -> Encode.Value
encodeItem item =
    object
        [ ( "itemId", Encode.string (itemId item) )
        , ( "listId", Encode.string (listIdToString item.listId) )
        , ( "text", Encode.string item.text )
        , ( "completed", Encode.bool item.completed )
        ]


emptyObject : Encode.Value
emptyObject =
    object []



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

                    "UpdateList" ->
                        decodeUpdatedListEvent

                    "DeleteList" ->
                        decodeDeletedListEvent

                    "CreateItem" ->
                        decodeCreatedItemEvent

                    "UpdateItem" ->
                        decodeUpdatedItemEvent

                    other ->
                        fail <| "Unknown event received from server: " ++ other
            )


decodeRegisteredEvent : Decoder Event
decodeRegisteredEvent =
    map Registered
        (at [ "confirmAction", "data", "clientId" ] (map ClientId uuid))


listsDecoder : Decoder (List ShoppingList)
listsDecoder =
    at [ "confirmAction", "data", "lists" ] (Decode.list decodeShoppingList)


decodeGotListsEvent : Decoder Event
decodeGotListsEvent =
    map GotLists listsDecoder


decodeCreatedListEvent : Decoder Event
decodeCreatedListEvent =
    map CreatedList listsDecoder


decodeDeletedListEvent : Decoder Event
decodeDeletedListEvent =
    map DeletedList listsDecoder


decodeUpdatedListEvent : Decoder Event
decodeUpdatedListEvent =
    map UpdatedListTitle listsDecoder


decodeCreatedItemEvent : Decoder Event
decodeCreatedItemEvent =
    map CreatedItem listsDecoder


decodeUpdatedItemEvent : Decoder Event
decodeUpdatedItemEvent =
    map UpdatedItemText listsDecoder


decodeShoppingList : Decoder ShoppingList
decodeShoppingList =
    decode ShoppingList
        |> required "listId" (map ListId uuid)
        |> required "title" Decode.string
        |> required "items" (Decode.list decodeItem)


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "itemId" (map ItemId uuid)
        |> required "text" Decode.string
        |> required "completed" Decode.bool
        |> required "listsId" (map ListId uuid)


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
