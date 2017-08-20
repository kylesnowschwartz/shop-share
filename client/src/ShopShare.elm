module ShopShare exposing (init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck, onClick, onFocus)
import List.Extra exposing (..)
import WebSocket as WS
import Action exposing (publishAction)
import Config exposing (wsAddress)
import Event exposing (handleEvent)
import JSON
import Types exposing (..)
import UpdateHelpers exposing (..)
import UuidHelpers exposing (..)


-- MODEL


init : Int -> ( Model, Cmd Msg )
init randomNumber =
    { shoppingLists = []
    , clientId = Nothing
    , errorMessage = Nothing
    , uuidSeed = uuidSeedFromInt randomNumber
    }
        ! [ publishAction Register ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateListClicked ->
            let
                ( newModel, newList ) =
                    createListWithNewUuid model
            in
                newModel ! [ publishAction (CreateList newList) ]

        DeleteListClicked list ->
            (deleteList model list) ! [ publishAction (DeleteList list) ]

        CreateItemClicked list ->
            let
                ( newModel, newItem ) =
                    createItemWithNewUuid model list
            in
                newModel ! [ publishAction (CreateItem newItem) ]

        ListTitleEdited list newTitle ->
            let
                ( newModel, updatedList ) =
                    editListTitle model list newTitle
            in
                newModel ! [ publishAction (UpdateList updatedList) ]

        ItemTextEdited list item newItemText ->
            let
                updatedItem =
                    { item | text = newItemText }
            in
                replaceList (updateItem updatedItem list) model
                    ! [ publishAction (UpdateItem updatedItem) ]

        ItemChecked list item itemChecked ->
            let
                updatedItem =
                    { item | completed = itemChecked }
            in
                replaceList (updateItem updatedItem list) model
                    ! [ publishAction (UpdateItem updatedItem) ]

        DeleteItemClicked list deletedItem ->
            replaceList (deleteItem deletedItem list) model
                ! [ publishAction (DeleteItem deletedItem) ]

        ClearCheckedItems list ->
            let
                updatedList =
                    { list
                        | listItems = List.Extra.filterNot .completed list.listItems
                    }
            in
                replaceList updatedList model ! []

        WSMessageReceived message ->
            handleMessage model message


handleMessage : Model -> String -> ( Model, Cmd Msg )
handleMessage model message =
    case JSON.decodeEvent message of
        Ok event ->
            handleEvent model event

        Err err ->
            { model | errorMessage = Just err } ! []



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
        [ input [ tabindex 2, placeholder "Item name", value item.text, onInput (ItemTextEdited list item) ] []
        , label [ class "checkbox" ]
            [ input [ type_ "checkbox", checked item.completed, Html.Events.onCheck (ItemChecked list item) ] [] ]
        , a [ tabindex -1, class "delete is-small", onClick (DeleteItemClicked list item) ] []
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
