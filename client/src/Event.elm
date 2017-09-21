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
                replaceLists lists model ! []

            CreatedList list ->
                addList list model ! []

            UpdatedListTitle list ->
                replaceList list model ! []

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
