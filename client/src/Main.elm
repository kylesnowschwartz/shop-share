module Main exposing (main)

import Html
import ShopShare exposing (init, subscriptions, update, view)
import Types exposing (Model, Msg)


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
