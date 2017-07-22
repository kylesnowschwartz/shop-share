module ShopShare exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)


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
    div [] [ text "Hello shop share!" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
