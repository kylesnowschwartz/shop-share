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


type alias ListItem =
    { text : String
    , completed : Bool
    }


init : ( Model, Cmd Msg )
init =
    { shoppingLists =
        [ { id = 0
          , name = ""
          , listItems =
                [ { text = ""
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
    | ItemAdded
    | ItemTextEdited


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShoppingListNameEdited updatedId newName ->
            { model | shoppingLists = List.map (updateName updatedId newName) model.shoppingLists } ! []

        ItemAdded ->
            model ! []

        ItemTextEdited ->
            model ! []


updateName : ShoppingListId -> String -> ShoppingList -> ShoppingList
updateName updatedId newName list =
    if list.id == updatedId then
        { list | name = newName }
    else
        list



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
            [ ul [ class "list" ] (List.map viewListItem list.listItems)
            ]
        ]


viewListItem : ListItem -> Html Msg
viewListItem item =
    li []
        [ input [ placeholder "Item name" ] []
        , input [ type_ "checkbox", name "list-item", value item.text ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
