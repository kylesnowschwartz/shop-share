module Main exposing (main)

import Html
import ShopShare exposing (Msg, init, subscriptions, update, view)
import Types exposing (Model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
