port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api.Api exposing (HttpData(..))
import Api.CurrencyRate exposing (CurrencyRate)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, selected, type_, value)
import Html.Events exposing (onInput)
import Http
import Session exposing (Session)
import View.Footer as Footer



-- MODEL


type alias Model =
    { session : Session
    , from : String
    , to : String
    , amount : Float
    , currencies : HttpData String (List CurrencyRate)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        cmd =
            Api.CurrencyRate.getCurrencyRates GotCurrencyRates
    in
    ( { session = session
      , from = "BRL"
      , to = "EUR"
      , amount = 1
      , currencies = Loading
      }
    , cmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Conversor de moedas"
    , body =
        [ div [ class "flex justify-center pt-10 pb-5" ]
            [ div [ class "w-full max-w-xs" ]
                [ h1 [ class "text-center text-2xl mb-6" ] [ text "Conversor de Moedas" ]
                , form [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ]
                    (case model.currencies of
                        Success currencies ->
                            let
                                result =
                                    convertCurrency model.amount model.from model.to currencies
                            in
                            [ div [ class "mb-4" ]
                                [ label [ class "block text-gray-700 text-sm font-bold mb-2" ] [ text "Moeda de origem" ]
                                , div [ class "relative" ]
                                    [ select
                                        [ class selectClasses, value model.from, onInput ChangeOriginCurrency ]
                                        [ option [ value "BRL", selected (model.from == "BRL") ] [ text "Real" ]
                                        , option [ value "USD", selected (model.from == "USD") ] [ text "Dólar americano" ]
                                        , option [ value "EUR", selected (model.from == "EUR") ] [ text "Euro" ]
                                        ]
                                    ]
                                ]
                            , div [ class "mb-4" ]
                                [ label [ class "block text-gray-700 text-sm font-bold mb-2" ]
                                    [ text "Moeda de destino" ]
                                , div [ class "relative" ]
                                    [ select
                                        [ class selectClasses, value model.to, onInput ChangeDestinyCurrency ]
                                        [ option [ value "USD", selected (model.to == "USD") ] [ text "Dólar americano" ]
                                        , option [ value "BRL", selected (model.to == "BRL") ] [ text "Real" ]
                                        , option [ value "EUR", selected (model.to == "EUR") ] [ text "Euro" ]
                                        ]
                                    ]
                                ]
                            , div [ class "mb-6" ]
                                [ label [ class "block text-gray-700 text-sm font-bold mb-2" ]
                                    [ text "Quantidade" ]
                                , input [ type_ "number", onInput ChangeAmount, value (String.fromFloat model.amount), class "shadow appearence-none border rounded w-full py-2 px-3 text-gray" ] []
                                ]
                            , div [ class "flex w-full" ]
                                [ button [ class "bg-blue-500 w-full hover:bg-blue-700 text-white font-bold py-2 px-4" ] [ text "Converter" ] ]
                            , div [ class "flex w-full text-center mt-5 text-gray-700 text-sm" ]
                                [ text ("Convertendo " ++ String.fromFloat model.amount ++ " " ++ model.from ++ " para " ++ model.to ++ " totalizando " ++ String.fromFloat result ++ " " ++ model.to) ]
                            ]

                        Loading ->
                            [ div [ class "text-center" ] [ text "Carregando..." ] ]

                        Error error ->
                            [ div [ class "text-center text-red-700" ] [ text error ] ]
                    )
                ]
            ]
        , Footer.view
        ]
    }



-- UPDATE


type Msg
    = ChangeOriginCurrency String
    | ChangeDestinyCurrency String
    | ChangeAmount String
    | GotCurrencyRates (Result Http.Error (List CurrencyRate))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOriginCurrency currencyCode ->
            ( { model | from = currencyCode }, Cmd.none )

        ChangeDestinyCurrency currencyCode ->
            ( { model | to = currencyCode }, Cmd.none )

        ChangeAmount amount ->
            case String.toFloat amount of
                Just value ->
                    ( { model | amount = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotCurrencyRates response ->
            case response of
                Ok data ->
                    ( { model | currencies = Success data }, saveCurrencies data )

                Err _ ->
                    ( { model | currencies = Error "Erro ao carregar as moedas" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port saveCurrencies : List CurrencyRate -> Cmd msg



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


selectClasses : String
selectClasses =
    "block appearance-none w-full border shadow py-2 px-3 pr-8 rounded"


convertCurrency : Float -> String -> String -> List CurrencyRate -> Float
convertCurrency amount from to currencies =
    let
        destinyCurrencyValue =
            if from == to then
                1

            else
                List.filter (\currency -> currency.base == from) currencies
                    |> List.head
                    |> Maybe.map (getCurrencyValue to)
                    |> Maybe.withDefault 0
    in
    destinyCurrencyValue * amount


getCurrencyValue : String -> CurrencyRate -> Float
getCurrencyValue currencyCode currencyRate =
    let
        maybeValue =
            case currencyCode of
                "USD" ->
                    currencyRate.rates.usd

                "EUR" ->
                    currencyRate.rates.eur

                "BRL" ->
                    currencyRate.rates.brl

                _ ->
                    Just 0
    in
    Maybe.withDefault 0 maybeValue
