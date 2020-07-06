module Page.About exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Session exposing (Session)
import View.Footer as Footer



-- MODEL


type alias Model =
    { session : Session
    , link : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, link = "https://www.fidelisclayton.dev/series/elm-na-pratica" }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Sobre"
    , body =
        [ div [ class "flex justify-center pt-10 pb-5" ]
            [ div [ class "w-full max-w-xs" ]
                [ h1 [ class "text-center text-2xl mb-6" ] [ text "Sobre" ]
                , div [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ]
                    [ p []
                        [ text "Um simples conversor de moedas criado na série de tutoriais "
                        , a [ href model.link, class "text-blue-600" ] [ text "Elm na prática" ]
                        , text "."
                        ]
                    ]
                ]
            ]
        , Footer.view
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
toSession =
    .session
