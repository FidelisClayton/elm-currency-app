port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DateFormat
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, selected, type_, value)
import Html.Events exposing (onInput)
import Http
import Iso8601
import Json.Decode
import Json.Encode
import Route
import Time exposing (Posix)
import Url


apiUrl : String
apiUrl =
    "https://elm-currency-api.herokuapp.com"


selectClasses : String
selectClasses =
    "block appearance-none w-full border shadow py-2 px-3 pr-8 rounded"


type alias ConversionRate =
    { usd : Maybe Float
    , eur : Maybe Float
    , brl : Maybe Float
    }


type alias CurrencyRate =
    { base : String
    , date : String
    , rates : ConversionRate
    }


type alias HistoryItem =
    { date : Posix
    , rate : Float
    }


conversionRateDecoder : Json.Decode.Decoder ConversionRate
conversionRateDecoder =
    Json.Decode.map3 ConversionRate
        (Json.Decode.maybe (Json.Decode.field "USD" Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.field "EUR" Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.field "BRL" Json.Decode.float))


currencyRateDecoder : Json.Decode.Decoder CurrencyRate
currencyRateDecoder =
    Json.Decode.map3 CurrencyRate
        (Json.Decode.field "base" Json.Decode.string)
        (Json.Decode.field "date" Json.Decode.string)
        (Json.Decode.field "rates" conversionRateDecoder)


historyItemDecoder : Json.Decode.Decoder HistoryItem
historyItemDecoder =
    Json.Decode.map2 HistoryItem
        (Json.Decode.field "date" Iso8601.decoder)
        (Json.Decode.field "rate" Json.Decode.float)


type HttpData error data
    = Loading
    | Success data
    | Error error


type alias Model =
    { from : String
    , to : String
    , amount : Float
    , currencies : HttpData String (List CurrencyRate)
    , key : Nav.Key
    , url : Url.Url
    , history : HttpData String (List HistoryItem)
    }


init : Json.Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currencies =
            case Json.Decode.decodeValue (Json.Decode.list currencyRateDecoder) flags of
                Ok decodedCurrencies ->
                    Success decodedCurrencies

                _ ->
                    Loading

        cmd =
            case Route.fromUrl url of
                Route.History from to ->
                    getHistory from to

                Route.Home ->
                    getCurrencyRates

                _ ->
                    Cmd.none
    in
    ( { from = "BRL"
      , to = "EUR"
      , amount = 1
      , currencies = currencies
      , key = key
      , url = url
      , history = Loading
      }
    , cmd
    )


type Msg
    = ChangeOriginCurrency String
    | ChangeDestinyCurrency String
    | ChangeAmount String
    | GotCurrencyRates (Result Http.Error (List CurrencyRate))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHistory (Result Http.Error (List HistoryItem))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                cmd =
                    case Route.fromUrl url of
                        Route.History from to ->
                            getHistory from to

                        _ ->
                            Cmd.none
            in
            ( { model | url = url }, cmd )

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

        GotHistory response ->
            case response of
                Ok data ->
                    ( { model | history = Success data }, Cmd.none )

                Err _ ->
                    ( { model | history = Error "Erro ao carregar o histórico" }, Cmd.none )


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


viewHome : Model -> Browser.Document Msg
viewHome model =
    { title = "Conversor de moedas"
    , body =
        [ div [ class "flex justify-center py-10" ]
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
                , a [ href "/", class "mx-2" ] [ text "Conversor" ]
                , a [ href "/history/from/EUR/to/BRL", class "mx-2" ] [ text "Histórico de BRL x EUR" ]
                ]
            ]
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


viewHistory : String -> String -> Model -> Browser.Document Msg
viewHistory from to model =
    { title = "Histórico"
    , body =
        [ div [ class "flex justify-center py-10" ]
            [ div [ class "w-full max-w-sm" ]
                [ h1 [ class "text-center text-2xl mb-6" ] [ text <| "Histórico " ++ from ++ " x " ++ to ]
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
        ]
    }


viewNotFound : Model -> Browser.Document Msg
viewNotFound model =
    { title = "404"
    , body =
        [ text "Página não encontrada"
        , div [ class "mt-2" ]
            [ a [ href "/" ] [ text "Conversor" ]
            ]
        ]
    }


view : Model -> Browser.Document Msg
view model =
    case Route.fromUrl model.url of
        Route.NotFound ->
            viewNotFound model

        Route.History from to ->
            viewHistory from to model

        Route.Home ->
            viewHome model


getCurrencyRates : Cmd Msg
getCurrencyRates =
    Http.get
        { url = apiUrl ++ "/v1/latest"
        , expect = Http.expectJson GotCurrencyRates (Json.Decode.list currencyRateDecoder)
        }


getHistory : String -> String -> Cmd Msg
getHistory from to =
    Http.get
        { url = apiUrl ++ "/v1/history?from=" ++ from ++ "&to=" ++ to
        , expect = Http.expectJson GotHistory (Json.Decode.list historyItemDecoder)
        }


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port saveCurrencies : List CurrencyRate -> Cmd msg


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
