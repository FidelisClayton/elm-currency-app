module Api.Api exposing (HttpData(..), apiUrl)

-- TYPES


type HttpData error data
    = Loading
    | Success data
    | Error error



-- HELPERS


apiUrl : String
apiUrl =
    "https://elm-currency-api.herokuapp.com"
