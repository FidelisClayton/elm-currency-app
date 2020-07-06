module Api.CurrencyRate exposing (ConversionRate, CurrencyRate, currencyRateDecoder, getCurrencyRates)

import Api.Api exposing (apiUrl)
import Http
import Json.Decode


apiUrl : String
apiUrl =
    "https://elm-currency-api.herokuapp.com"



-- TYPES


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



-- DECODERS


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



-- REQUESTS


getCurrencyRates : (Result Http.Error (List CurrencyRate) -> msg) -> Cmd msg
getCurrencyRates msg =
    Http.get
        { url = apiUrl ++ "/v1/latest"
        , expect = Http.expectJson msg (Json.Decode.list currencyRateDecoder)
        }
