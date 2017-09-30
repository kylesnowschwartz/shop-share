module Debouncers exposing (..)

import Debounce exposing (Config, later)
import Types exposing (..)
import Time exposing (second)


debounceListTitleConfig : Config Msg
debounceListTitleConfig =
    { strategy = later <| 0.5 * second
    , transform = DebounceListTitle
    }


debounceItemTextConfig : Config Msg
debounceItemTextConfig =
    { strategy = later <| 0.5 * second
    , transform = DebounceItemText
    }


pushListEditIntoDebouncer : Model -> ShoppingList -> ( Model, Cmd Msg )
pushListEditIntoDebouncer model list =
    let
        ( debouncer, debouncerCmd ) =
            Debounce.push debounceListTitleConfig list model.listDebouncer
    in
        { model | listDebouncer = debouncer } ! [ debouncerCmd ]


pushItemEditIntoDebouncer : Model -> Item -> ( Model, Cmd Msg )
pushItemEditIntoDebouncer model item =
    let
        ( debouncer, debouncerCmd ) =
            Debounce.push debounceItemTextConfig item model.itemDebouncer
    in
        { model | itemDebouncer = debouncer } ! [ debouncerCmd ]


performDebouncedListEdit :
    Model
    -> Debounce.Msg
    -> (ShoppingList -> Cmd Msg)
    -> ( Model, Cmd Msg )
performDebouncedListEdit model msg cmd =
    let
        ( debouncer, debouncedCmd ) =
            Debounce.update
                debounceListTitleConfig
                (Debounce.takeLast cmd)
                msg
                model.listDebouncer
    in
        { model | listDebouncer = debouncer } ! [ debouncedCmd ]


performDebouncedItemEdit :
    Model
    -> Debounce.Msg
    -> (Item -> Cmd Msg)
    -> ( Model, Cmd Msg )
performDebouncedItemEdit model msg cmd =
    let
        ( newDebounce, debouncedCmd ) =
            Debounce.update
                debounceItemTextConfig
                (Debounce.takeLast cmd)
                msg
                model.itemDebouncer
    in
        { model | itemDebouncer = newDebounce } ! [ debouncedCmd ]
