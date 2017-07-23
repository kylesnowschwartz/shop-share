module ShopShare exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- MODEL


type alias Model =
    { shoppingLists : List ShoppingList }


type alias ShoppingList =
    { id : ShoppingListId
    , name : String
    , listItems : List ListItem
    }


type alias ShoppingListId =
    Int


type alias ItemId =
    Int


type alias ListItem =
    { id : ItemId
    , text : String
    , completed : Bool
    }


init : ( Model, Cmd Msg )
init =
    { shoppingLists =
        [ { id = 1
          , name = "Groceries"
          , listItems =
                [ { id = 1
                  , text = "Apples"
                  , completed = False
                  }
                ]
          }
        ]
    }
        ! []



-- UPDATE


type Msg
    = ShoppingListNameEdited Int String
    | ItemAdded Int String
    | ItemTextEdited


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShoppingListNameEdited updatedId newName ->
            { model | shoppingLists = updateShoppingList model updatedId (updateName newName) } ! []

        ItemAdded updatedId newItem->
            { model | shoppingLists = updateShoppingList model updatedId (addItem newItem) } ! []

        ItemTextEdited ->
            model ! []


updateName : String -> ShoppingList -> ShoppingList
updateName newName list =
    { list | name = newName }

addItem : String -> ShoppingList -> ShoppingList
addItem newItem list =
  { list | listItems = list.listItems ++ [{id = (incrementItemId list), text = newItem, completed = False}] }
  -- TODO
  -- this is currently adding an item on the first input, resulting in 1 letter items
  -- instead, we need to be editing an item on input
  -- then when you go to the blank box, you add a new blank item, and typing edits that item


incrementItemId : ShoppingList -> Int
incrementItemId list =
  Maybe.withDefault 0 (List.maximum (List.map .id list.listItems)) + 1



updateShoppingList : Model -> ShoppingListId -> (ShoppingList -> ShoppingList) -> List ShoppingList
updateShoppingList model updatedId updateFunction =
    let
        filteredUpdate list =
            if list.id == updatedId then
                updateFunction list
            else
                list
    in
        List.map filteredUpdate model.shoppingLists



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello shop share!" ]
        , ol [ id "lists" ] (List.map viewShoppingList model.shoppingLists)
        ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    li []
        [ input
            [ placeholder "List name"
            , onInput (ShoppingListNameEdited list.id)
            , value list.name
            ]
            []
        , div []
            [ ul [ class "list" ] (List.concat [List.map viewListItem list.listItems, [viewAddListItem list]])
            ]
        ]


viewListItem : ListItem -> Html Msg
viewListItem item =
    li []
        [ input [ placeholder "Item name", value item.text ] []
        , input [ type_ "checkbox", name "list-item" ] []
        ]

viewAddListItem : ShoppingList -> Html Msg
viewAddListItem list =
  li []
      [ input
          [ placeholder "Add a new list item"
          , onInput (ItemAdded list.id)
          ] [] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
