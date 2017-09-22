module Event exposing (..)

import Action exposing (..)
import Types exposing (..)
import UpdateHelpers exposing (..)
import Ports exposing (focusItemInputPort)
import UuidHelpers exposing (..)


handleEvent : Model -> Event -> ( Model, Cmd Msg )
handleEvent model event =
    let
        fetchInitialLists id =
            { model | clientId = Just id }
    in
        case event of
            Registered id ->
                fetchInitialLists id ! [ publishAction GetLists ]

            GotLists lists ->
                { model | lists = lists } ! []

            CreatedList list ->
                { model | lists = replaceById list model.lists } ! []

            UpdatedList list ->
                { model | lists = replaceById (Debug.log "List: " list) model.lists } ! []

            -- TODO: Need to send listId along with DeletedList so clients can remove.
            DeletedList ->
                model ! []

            CreatedItem item ->
                addItem item model ! [ focusItemInput item ]

            UpdatedItemText item ->
                replaceItem item model ! []

            DeletedItem ->
                model ! []


focusItemInput : Item -> Cmd Msg
focusItemInput item =
    focusItemInputPort <| itemId item
