module Page.History exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api.Api exposing (HttpData(..))
import Api.History exposing (HistoryItem)
import Browser
import DateFormat
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Session exposing (Session)
import Time exposing (Posix)
import View.Footer as Footer



-- MODEL


type alias Model =
    { session : Session
    , from : String
    , to : String
    , history : HttpData String (List HistoryItem)
    }


init : Session -> String -> String -> ( Model, Cmd Msg )
init session from to =
    ( { session = session
      , from = from
      , to = to
      , history = Loading
      }
    , Api.History.getHistory GotHistory from to
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Histórico"
    , body =
        [ div [ class "flex justify-center pt-10 pb-5" ]
            [ div [ class "w-full max-w-sm" ]
                [ h1 [ class "text-center text-2xl mb-6" ] [ text <| "Histórico " ++ model.from ++ " x " ++ model.to ]
                , div [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ]
                    [ case model.history of
                        Loading ->
                            text "Carregando"

                        Success history ->
                            viewHistoryTable history

                        Error error ->
                            text error
                    ]
                ]
            ]
        , Footer.view
        ]
    }


viewHistoryRow : HistoryItem -> Html Msg
viewHistoryRow historyItem =
    tr []
        [ td [ class "text-left" ] [ text <| formatPosix historyItem.date ]
        , td [ class "text-left" ] [ text <| String.fromFloat historyItem.rate ]
        ]


viewHistoryTable : List HistoryItem -> Html Msg
viewHistoryTable history =
    table [ class "table-fixed w-full" ]
        [ thead []
            [ tr []
                [ th [ class "w-3/4 text-left" ] [ text "Data" ]
                , th [ class "w-1/4 text-left" ] [ text "Valor" ]
                ]
            ]
        , tbody []
            (List.take 30 history |> List.map viewHistoryRow)
        ]



-- UPDATE


type Msg
    = GotHistory (Result Http.Error (List HistoryItem))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHistory response ->
            case response of
                Ok data ->
                    ( { model | history = Success data }, Cmd.none )

                Err _ ->
                    ( { model | history = Error "Erro ao carregar o histórico" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


formatPosix : Posix -> String
formatPosix =
    DateFormat.format
        [ DateFormat.dayOfMonthFixed
        , DateFormat.text "/"
        , DateFormat.monthFixed
        , DateFormat.text "/"
        , DateFormat.yearNumber
        ]
        Time.utc
