module CssHelpers exposing (activeClassIfJust)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


activeClassIfJust : Maybe a -> String -> Attribute msg
activeClassIfJust predicate baseClass =
    case predicate of
        Nothing ->
            class baseClass

        Just list ->
            class <| baseClass ++ " is-active"
