module ShopShare exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    { shopping_lists : List ShoppingList }


type alias ShoppingList =
    { name : String, list_items : List ListItem }


type alias ListItem =
    { text : String
    , completed : Bool
    }


init : ( Model, Cmd Msg )
init =
    { shopping_lists = [{name = "", list_items =[]}] } ! []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello shop share!" ]
        , div [ id "create-list-form" ] [ Html.form [ class "form" ] [ input [ placeholder "enter some text" ] [] ] ]
        , ol [ id "lists" ] (List.map viewShoppingList model.shopping_lists)
        ]


viewShoppingList : ShoppingList -> Html Msg
viewShoppingList list =
    li []
        [ text list.name
        , div []
            [ input [ placeholder "enter some text" ] []
            , ul [ class "list" ] (List.map viewListItem list.list_items)
            ]
        ]


viewListItem : ListItem -> Html Msg
viewListItem item =
    li [] [ text item.text, input [ type_ "checkbox", name "list-item", value item.text ] [] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
