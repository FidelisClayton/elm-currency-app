module Route exposing (Route(..), fromUrl, routeParser)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


type Route
    = Home
    | History String String
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map History (Parser.s "history" </> Parser.s "from" </> Parser.string </> Parser.s "to" </> Parser.string)
        ]


fromUrl : Url -> Route
fromUrl url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFound
