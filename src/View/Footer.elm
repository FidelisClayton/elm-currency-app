module View.Footer exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, href)


view : Html msg
view =
    footer
        [ class "flex flex-col" ]
        [ a [ class "text-blue-600", href "/" ] [ text "Home" ]
        , a [ class "text-blue-600", href "/about" ] [ text "Sobre" ]
        , a [ class "text-blue-600", href "/history/from/BRL/to/EUR", class "mx-2" ] [ text "Histórico de BRL x EUR" ]
        , a [ class "text-blue-600", href "/history/from/BRL/to/USD", class "mx-2" ] [ text "Histórico de BRL x USD" ]
        , a [ class "text-blue-600", href "/history/from/EUR/to/BRL", class "mx-2" ] [ text "Histórico de EUR x BRL" ]
        , a [ class "text-blue-600", href "/history/from/EUR/to/USD", class "mx-2" ] [ text "Histórico de EUR x USD" ]
        , a [ class "text-blue-600", href "/history/from/USD/to/BRL", class "mx-2" ] [ text "Histórico de USD x BRL" ]
        , a [ class "text-blue-600", href "/history/from/USD/to/EUR", class "mx-2" ] [ text "Histórico de USD x EUR" ]
        ]
