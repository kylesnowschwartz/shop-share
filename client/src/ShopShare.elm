module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import JSON
import Json.Encode as Encode
import List.Extra exposing (..)
import Types exposing (..)
import Uuid as Uuid exposing (Uuid)
import UuidHelpers exposing (..)
import WebSocket as WS


-- MODEL


wsAddress : String
wsAddress =
    "ws://localhost:8000"


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { shoppingLists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    }
        ! [ WS.send wsAddress (JSON.encodeMsg Register JSON.emptyObject) ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Register ->
            model ! []

        GetLists ->
            model ! [ sendMsg GetLists JSON.emptyObject ]

        CreateNewList ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model
            in
                { modelWithNewSeed | shoppingLists = addList newUuid model.shoppingLists }
                    ! [ sendMsg msg JSON.emptyObject ]

        DeleteList list ->
            { model | shoppingLists = List.Extra.remove list model.shoppingLists }
                ! [ sendMsg msg (JSON.encodeList list) ]

        ShoppingListTitleEdited list newTitle ->
            { model | shoppingLists = updateShoppingList model list (updateTitle newTitle) }
                ! [ sendMsg msg (JSON.encodeList list) ]

        ItemAdded list ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model

                newItem =
                    { id = ItemId newUuid, text = "", completed = False }
            in
                { modelWithNewSeed | shoppingLists = updateShoppingList model list (addItem newItem) }
                    ! [ sendMsg msg (JSON.encodeItem newItem list) ]

        ItemTextEdited listId item newItemText ->
            { model | shoppingLists = updateShoppingList model listId (editItem newItemText item.id) }
                ! [ sendMsg msg (JSON.encodeItem { item | text = newItemText } listId) ]

        ItemChecked updatedListId item itemChecked ->
            { model | shoppingLists = updateShoppingList model updatedListId (checkItem itemChecked item) }
                ! []

        ItemDeleted updatedListId deletedItem ->
            { model | shoppingLists = updateShoppingList model updatedListId (deleteItem deletedItem) }
                ! []

        ClearCheckedItems updatedListId ->
            { model | shoppingLists = updateShoppingList model updatedListId (clearCheckedItems) }
                ! []

        MessageReceived message ->
            handleMessage model message


sendMsg : Msg -> Encode.Value -> Cmd Msg
sendMsg msg data =
    WS.send wsAddress (JSON.encodeMsg msg data)


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    let
        fetchAllLists =
            model ! [ WS.send wsAddress (JSON.encodeMsg GetLists JSON.emptyObject) ]
    in
        case JSON.decodeEvent message of
            Ok action ->
                case action of
                    Registered newId ->
                        { model | clientId = Just newId }
                            ! [ WS.send wsAddress (JSON.encodeMsg GetLists JSON.emptyObject) ]

                    GotLists lists ->
                        { model | shoppingLists = orderListsAndTheirItems lists } ! []

                    CreatedList newList ->
                        fetchAllLists

                    DeletedList _ ->
                        fetchAllLists

                    UpdatedListTitle _ ->
                        fetchAllLists

                    CreatedItem _ ->
                        fetchAllLists

                    UpdatedItemText _ ->
                        fetchAllLists

            Err err ->
                { model | errorMessage = Just err } ! []


orderListsAndTheirItems : List ShoppingList -> List ShoppingList
orderListsAndTheirItems =
    (List.sortBy listIdToString) << (List.map sortItems)


sortItems : ShoppingList -> ShoppingList
sortItems list =
    { list | listItems = List.sortBy itemIdToString list.listItems }


clearCheckedItems : ShoppingList -> ShoppingList
clearCheckedItems list =
    { list | listItems = List.Extra.filterNot .completed list.listItems }


updateTitle : String -> ShoppingList -> ShoppingList
updateTitle newTitle list =
    { list | title = newTitle }


addItem : Item -> ShoppingList -> ShoppingList
addItem item list =
    { list | listItems = list.listItems ++ [ item ] }


addList : Uuid -> List ShoppingList -> List ShoppingList
addList newUuid lists =
    lists ++ [ { id = ShoppingListId newUuid, title = "", listItems = [] } ]


editItem : String -> ItemId -> ShoppingList -> ShoppingList
editItem newItemText newItemId list =
    let
        applyIfEdited item =
            if item.id == newItemId then
                { item | text = newItemText }
            else
                item
    in
        { list | listItems = List.map applyIfEdited list.listItems }


checkItem : Bool -> Item -> ShoppingList -> ShoppingList
checkItem itemChecked itemToCheck list =
    let
        applyIfChecked item =
            if item.id == itemToCheck.id then
                { item | completed = itemChecked }
            else
                item
    in
        { list | listItems = List.map applyIfChecked list.listItems }


deleteItem : Item -> ShoppingList -> ShoppingList
deleteItem deletedItem list =
    { list | listItems = List.Extra.remove deletedItem list.listItems }


updateShoppingList : Model -> ShoppingList -> (ShoppingList -> ShoppingList) -> List ShoppingList
updateShoppingList model updatedList updateFunction =
    let
        filteredUpdate list =
            if list.id == updatedList.id then
                updateFunction list
            else
                list
    in
        List.map filteredUpdate model.shoppingLists



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ h1 [] [ text "Hello shop share!" ]
        , dl [ id "container lists" ] (List.map viewShoppingList model.shoppingLists)
        , div [ class "section" ]
            [ a [ onClick (CreateNewList) ] [ text "make a new list" ]
            ]
        , viewErrors model
        , viewClientId model
        ]


viewErrors : Model -> Html Msg
viewErrors model =
    div [] [ text (Maybe.withDefault "" model.errorMessage) ]


viewClientId : Model -> Html Msg
viewClientId model =
    case model.clientId of
        Nothing ->
            div [] []

        Just id ->
            h3 [] [ text ("Registered with server as client " ++ toString id) ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    dd [ class "content" ]
        [ input
            [ placeholder "List title"
            , onInput (ShoppingListTitleEdited list)
            , value list.title
            ]
            []
        , a [ tabindex -1, class "delete is-small", onClick (DeleteList list) ] []
        , div []
            [ dl [ class "list" ]
                (List.concat [ List.map (viewListItem list) list.listItems, [ viewAddListItem list ] ])
            ]
        , div []
            [ viewClearCheckedItems list ]
        ]


viewListItem : ShoppingList -> Item -> Html Msg
viewListItem list item =
    dd []
        [ input [ tabindex 2, placeholder "Item name", value item.text, onInput (ItemTextEdited list item) ] []
        , label [ class "checkbox" ]
            [ input [ type_ "checkbox", checked item.completed, Html.Events.onCheck (ItemChecked list item) ] [] ]
        , a [ tabindex -1, class "delete is-small", onClick (ItemDeleted list item) ] []
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    dd []
        [ input
            [ placeholder "Add a new list item"
            , onClick (ItemAdded list)
            ]
            []
        ]


viewClearCheckedItems : ShoppingList -> Html Msg
viewClearCheckedItems list =
    button [ class "button is-primary", onClick (ClearCheckedItems list) ] [ text "clear checked items" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen wsAddress MessageReceived
