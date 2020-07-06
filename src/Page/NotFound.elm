module Page.NotFound exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Session exposing (Session)



-- MODEL


type alias Model =
    Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "404"
    , body =
        [ text "Página não encontrada"
        , div [ class "mt-2" ]
            [ a [ href "/" ] [ text "Home" ]
            ]
        ]
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model
