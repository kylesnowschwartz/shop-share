module JSON exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (..)
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
              , encodeActionTypeAndData action
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
        [ ( "listId", Encode.string <| listId list )
        , ( "title", Encode.string list.title )
        ]


encodeItem : Item -> Encode.Value
encodeItem item =
    object
        [ ( "itemId", Encode.string <| itemId item )
        , ( "listId", Encode.string <| listIdToString item.listId )
        , ( "text", Encode.string item.text )
        , ( "completed", Encode.bool item.completed )
        ]


emptyObject : Encode.Value
emptyObject =
    object []



-- DECODING


decodeEvent : String -> Result String Event
decodeEvent message =
    decodeString eventDecoder (Debug.log "Message: " message)


eventDecoder : Decoder Event
eventDecoder =
    (at [ "confirmAction", "type" ] Decode.string)
        |> andThen
            (\type_ ->
                case type_ of
                    "Register" ->
                        registeredEventDecoder

                    "GetLists" ->
                        gotListsEventDecoder

                    "CreateList" ->
                        createdListEventDecoder

                    "UpdateList" ->
                        updatedListEventDecoder

                    "DeleteList" ->
                        deletedListEventDecoder

                    "CreateItem" ->
                        createdItemEventDecoder

                    "UpdateItem" ->
                        updatedItemEventDecoder

                    "DeleteItem" ->
                        deletedItemEventDecoder

                    other ->
                        fail <| "Unknown event received from server: " ++ other
            )


registeredEventDecoder : Decoder Event
registeredEventDecoder =
    map Registered <|
        at [ "confirmAction", "data", "clientId" ] <|
            map ClientId uuid


actionConfirmationDecoder : String -> Decoder a -> Decoder a
actionConfirmationDecoder key decoder =
    at [ "confirmAction", "data", key ] <| decoder


gotListsEventDecoder : Decoder Event
gotListsEventDecoder =
    map GotLists <| actionConfirmationDecoder "lists" <| Decode.list listDecoder


createdListEventDecoder : Decoder Event
createdListEventDecoder =
    map CreatedList <| actionConfirmationDecoder "list" <| listDecoder


updatedListEventDecoder : Decoder Event
updatedListEventDecoder =
    map UpdatedList <| actionConfirmationDecoder "list" <| listDecoder


deletedListEventDecoder : Decoder Event
deletedListEventDecoder =
    succeed DeletedList


createdItemEventDecoder : Decoder Event
createdItemEventDecoder =
    map CreatedItem <| actionConfirmationDecoder "item" <| itemDecoder


updatedItemEventDecoder : Decoder Event
updatedItemEventDecoder =
    map UpdatedItemText <| actionConfirmationDecoder "item" <| itemDecoder


deletedItemEventDecoder : Decoder Event
deletedItemEventDecoder =
    succeed DeletedItem


listDecoder : Decoder ShoppingList
listDecoder =
    decode ShoppingList
        |> required "listId" (map ListId uuid)
        |> required "title" Decode.string
        |> required "items" (Decode.list itemDecoder)
        |> required "createdAt" (nullable date)
        |> required "updatedAt" (nullable date)


itemDecoder : Decoder Item
itemDecoder =
    decode Item
        |> required "itemId" (map ItemId uuid)
        |> required "text" Decode.string
        |> required "completed" Decode.bool
        |> required "listsId" (map ListId uuid)
        |> required "createdAt" (nullable date)
        |> required "updatedAt" (nullable date)


uuid : Decoder Uuid
uuid =
    Decode.string
        |> andThen
            (\str ->
                case Uuid.fromString str of
                    Just uuid_ ->
                        succeed uuid_

                    Nothing ->
                        fail str
            )
