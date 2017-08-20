module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import JSON
import List.Extra exposing (..)
import Types exposing (..)
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
        ! [ sendActionToServer Register ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateListClicked ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model

                newList =
                    { id = ListId newUuid, title = "", listItems = [] }
            in
                { modelWithNewSeed | shoppingLists = model.shoppingLists ++ [ newList ] }
                    ! [ sendActionToServer (CreateList newList) ]

        DeleteListClicked list ->
            { model | shoppingLists = List.Extra.remove list model.shoppingLists }
                ! [ sendActionToServer (DeleteList list) ]

        ListTitleEdited list newTitle ->
            { model | shoppingLists = updateShoppingList model list.id (updateTitle newTitle) }
                ! [ sendActionToServer (UpdateList list) ]

        CreateItemClicked list ->
            let
                ( modelWithNewSeed, newUuid ) =
                    stepUuid model

                newItem =
                    { id = ItemId newUuid, text = "", completed = False, listId = list.id }
            in
                { modelWithNewSeed | shoppingLists = updateShoppingList model list.id (addItem newItem) }
                    ! [ sendActionToServer (CreateItem newItem) ]

        -- TODO: Can simplify editItem etc. now that we're storing listId on Item.
        ItemTextEdited item newItemText ->
            { model | shoppingLists = updateShoppingList model item.listId (editItem newItemText item.id) }
                ! [ sendActionToServer (UpdateItem { item | text = newItemText }) ]

        ItemChecked item itemChecked ->
            { model | shoppingLists = updateShoppingList model item.listId (checkItem itemChecked item) }
                ! []

        DeleteItemClicked deletedItem ->
            { model | shoppingLists = updateShoppingList model deletedItem.listId (deleteItem deletedItem) }
                ! []

        ClearCheckedItems updatedList ->
            { model | shoppingLists = updateShoppingList model updatedList.id clearCheckedItems }
                ! []

        WSMessageReceived message ->
            handleMessage model message


createList : Model -> ( Model, Cmd Msg )
createList model =
    let
        ( modelWithNewSeed, newUuid ) =
            stepUuid model

        newList =
            { id = ListId newUuid, title = "", listItems = [] }
    in
        { modelWithNewSeed | shoppingLists = model.shoppingLists ++ [ newList ] }
            ! [ sendActionToServer (CreateList newList) ]


sendActionToServer : Action -> Cmd Msg
sendActionToServer action =
    WS.send wsAddress (JSON.encodeAction action)


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    let
        fetchAllLists =
            model ! [ sendActionToServer GetLists ]
    in
        case JSON.decodeEvent message of
            Ok action ->
                case action of
                    Registered newId ->
                        { model | clientId = Just newId }
                            ! [ sendActionToServer GetLists ]

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
    (List.sortBy listId) << (List.map sortItems)


sortItems : ShoppingList -> ShoppingList
sortItems list =
    { list | listItems = List.sortBy itemId list.listItems }


clearCheckedItems : ShoppingList -> ShoppingList
clearCheckedItems list =
    { list | listItems = List.Extra.filterNot .completed list.listItems }


updateTitle : String -> ShoppingList -> ShoppingList
updateTitle newTitle list =
    { list | title = newTitle }


addItem : Item -> ShoppingList -> ShoppingList
addItem item list =
    { list | listItems = list.listItems ++ [ item ] }


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


updateShoppingList : Model -> ListId -> (ShoppingList -> ShoppingList) -> List ShoppingList
updateShoppingList model updatedListId updateFunction =
    let
        filteredUpdate list =
            if list.id == updatedListId then
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
            [ a [ onClick (CreateListClicked) ] [ text "make a new list" ]
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
            , onInput (ListTitleEdited list)
            , value list.title
            ]
            []
        , a [ tabindex -1, class "delete is-small", onClick (DeleteListClicked list) ] []
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
        [ input [ tabindex 2, placeholder "Item name", value item.text, onInput (ItemTextEdited item) ] []
        , label [ class "checkbox" ]
            [ input [ type_ "checkbox", checked item.completed, Html.Events.onCheck (ItemChecked item) ] [] ]
        , a [ tabindex -1, class "delete is-small", onClick (DeleteItemClicked item) ] []
        ]


viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
    dd []
        [ input
            [ placeholder "Add a new list item"
            , onClick (CreateItemClicked list)
            ]
            []
        ]


viewClearCheckedItems : ShoppingList -> Html Msg
viewClearCheckedItems list =
    button [ class "button is-primary", onClick (ClearCheckedItems list) ] [ text "clear checked items" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen wsAddress WSMessageReceived
