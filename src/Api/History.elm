module Api.History exposing (HistoryItem, getHistory)

import Api.Api exposing (apiUrl)
import Http
import Iso8601
import Json.Decode
import Time exposing (Posix)



-- TYPES


type alias HistoryItem =
    { date : Posix
    , rate : Float
    }



-- DECODERS


historyItemDecoder : Json.Decode.Decoder HistoryItem
historyItemDecoder =
    Json.Decode.map2 HistoryItem
        (Json.Decode.field "date" Iso8601.decoder)
        (Json.Decode.field "rate" Json.Decode.float)



-- REQUESTS


getHistory : (Result Http.Error (List HistoryItem) -> msg) -> String -> String -> Cmd msg
getHistory msg from to =
    Http.get
        { url = apiUrl ++ "/v1/history?from=" ++ from ++ "&to=" ++ to
        , expect = Http.expectJson msg (Json.Decode.list historyItemDecoder)
        }
