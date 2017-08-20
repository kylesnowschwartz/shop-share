module Action exposing (..)

import Config exposing (..)
import JSON
import Types exposing (..)
import WebSocket as WS


publishAction : Action -> Cmd Msg
publishAction action =
    WS.send wsAddress (JSON.encodeAction action)
