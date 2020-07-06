module Session exposing (Session)

import Browser.Navigation exposing (Key)
import Url exposing (Url)



-- MODEL


type alias Session =
    { url : Url
    , key : Key
    }
