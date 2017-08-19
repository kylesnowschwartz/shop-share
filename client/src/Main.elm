module Main exposing (main)

import Html
import ShopShare exposing (Msg, init, subscriptions, update, view)
import Types exposing (Model)


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
