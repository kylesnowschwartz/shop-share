module ShopShare exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []



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
        , div [id "create-list-form"] [Html.form [class "form"] [input [placeholder "enter some text"] []]]
        , ol [ id "lists" ]
            [ li []
                [ text "Bunnings"
                , div [ id "Bunnings" ]
                    [ text "Bunnings"
                    , div [id "bunnings-add-item-form"] [Html.form [class "form"] [input [placeholder "enter some text"] []]]
                    , ul [ class "list" ]
                        [ li [] [ text "item1" ]
                        , li [] [ text "item2" ]
                        , li [] [ text "item3" ]
                        ]
                    ]
                ]
            , li []
                [ text "Groceries"
                , div [ id "Groceries" ]
                    [ text "Groceries"
                    , div [id "groceries-add-item-form"] [Html.form [class "form"] [input [placeholder "enter some text"] []]]
                    , ul [ class "list" ]
                        [ li [] [ text "item1" ]
                        , li [] [ text "item2" ]
                        , li [] [ text "item3" ]
                        ]
                    ]
                ]
            , li []
                [ text "Home"
                , div [ id "Home" ]
                    [ text "Home"
                    , div [id "home-add-item-form"] [Html.form [class "form"] [input [placeholder "enter some text"] []]]
                    , ul [ class "list" ]
                        [ li [] [ text "item1" ]
                        , li [] [ text "item2" ]
                        , li [] [ text "item3" ]
                        ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
