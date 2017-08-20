module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import JSON
import List.Extra exposing (..)
import Types exposing (..)
import UuidHelpers exposing (..)
import UpdateHelpers exposing (..)
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
                ( newModel, newList ) =
                    createListWithNewUuid model
            in
                newModel ! [ sendActionToServer (CreateList newList) ]

        DeleteListClicked list ->
            { model | shoppingLists = List.Extra.remove list model.shoppingLists }
                ! [ sendActionToServer (DeleteList list) ]

        CreateItemClicked list ->
            let
                ( newModel, newItem ) =
                    createItemWithNewUuid model list
            in
                newModel ! [ sendActionToServer (CreateItem newItem) ]

        ListTitleEdited list newTitle ->
            let
                updatedList =
                    { list | title = newTitle }
            in
                updateShoppingList updatedList model
                    ! [ sendActionToServer (UpdateList updatedList) ]

        ItemTextEdited item newItemText ->
            let
                updatedItem =
                    { item | text = newItemText }

                updatedList =
                    updateItem updatedItem (listForItem model item)
            in
                case updatedList of
                    Nothing ->
                        model ! []

                    Just list ->
                        updateShoppingList list model
                            ! [ sendActionToServer (UpdateItem updatedItem) ]

        ItemChecked item itemChecked ->
            let
                updatedItem =
                    { item | completed = itemChecked }

                updatedList =
                    updateItem updatedItem (listForItem model item)
            in
                case updatedList of
                    Nothing ->
                        model ! []

                    Just list ->
                        updateShoppingList list model
                            ! [ sendActionToServer (UpdateItem updatedItem) ]

        DeleteItemClicked deletedItem ->
            let
                updatedList =
                    deleteItem deletedItem (listForItem model deletedItem)
            in
                case updatedList of
                    Nothing ->
                        model ! []

                    Just list ->
                        updateShoppingList list model
                            ! [ sendActionToServer (DeleteItem deletedItem) ]

        ClearCheckedItems list ->
            let
                updatedList =
                    { list
                        | listItems = List.Extra.filterNot .completed list.listItems
                    }
            in
                updateShoppingList updatedList model ! []

        WSMessageReceived message ->
            handleMessage model message


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
                        { model | shoppingLists = orderListsAndTheirItems lists }
                            ! []

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
