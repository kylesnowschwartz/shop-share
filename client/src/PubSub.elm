module PubSub exposing (publish, processEvent)

import Config exposing (wsAddress)
import JSON
import Ports exposing (focusItemInput)
import Types exposing (..)
import UpdateHelpers exposing (replaceItem, replaceList)
import WebSocket as WS


publish : Action -> Cmd Msg
publish action =
    WS.send wsAddress <|
        JSON.encodeAction action


processEvent : Model -> Event -> ( Model, Cmd Msg )
processEvent model event =
    case event of
        Registered id ->
            { model | clientId = Just id } ! [ publish GetLists ]

        GotLists lists ->
            { model | lists = lists } ! []

        CreatedList list ->
            replaceList model list ! []

        UpdatedList list ->
            replaceList model list ! []

        -- TODO: Need to send listId along with DeletedList so clients can remove.
        DeletedList ->
            model ! []

        CreatedItem item ->
            replaceItem model item ! [ focusItemInput item ]

        UpdatedItemText item ->
            replaceItem model item ! []

        DeletedItem ->
            model ! []
