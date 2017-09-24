port module Ports exposing (focusItemInput, focusItemInputPort)

import Types exposing (Item, Msg)
import UuidHelpers exposing (itemId)


port focusItemInputPort : String -> Cmd msg


focusItemInput : Item -> Cmd Msg
focusItemInput item =
    itemId item |> focusItemInputPort
