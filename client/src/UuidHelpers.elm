module UuidHelpers exposing (..)

import Uuid as Uuid exposing (Uuid)
import Random.Pcg as Pcg
import Types exposing (..)


uuidSeedFromInt : Int -> Pcg.Seed
uuidSeedFromInt =
    Pcg.initialSeed


stepUuid : Model -> ( Model, Uuid.Uuid )
stepUuid model =
    let
        ( newUuid, newSeed ) =
            Pcg.step Uuid.uuidGenerator model.uuidSeed
    in
        ( { model | uuidSeed = newSeed }, newUuid )


listId : ShoppingList -> String
listId list =
    case list.id of
        ListId uuid ->
            Uuid.toString uuid


listIdToString : ListId -> String
listIdToString listId =
    case listId of
        ListId uuid ->
            Uuid.toString uuid


itemId : Item -> String
itemId item =
    case item.id of
        ItemId uuid ->
            Uuid.toString uuid


itemIdToString : ItemId -> String
itemIdToString itemId =
    case itemId of
        ItemId uuid ->
            Uuid.toString uuid
