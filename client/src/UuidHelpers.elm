module UuidHelpers exposing (..)

import Uuid as Uuid exposing (Uuid)
import Random.Pcg as Pcg
import Types exposing (..)


uuidSeedFromInt =
    Pcg.initialSeed


stepUuid : Model -> ( Model, Uuid.Uuid )
stepUuid model =
    let
        ( newUuid, newSeed ) =
            Pcg.step Uuid.uuidGenerator model.uuidSeed
    in
        ( { model | uuidSeed = newSeed }, newUuid )


listIdToString : ShoppingList -> String
listIdToString list =
    case list.id of
        ShoppingListId uuid ->
            Uuid.toString uuid


itemIdToString : Item -> String
itemIdToString item =
    case item.id of
        ItemId uuid ->
            Uuid.toString uuid
