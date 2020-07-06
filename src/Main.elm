module Main exposing (main)

import Api.CurrencyRate exposing (..)
import Api.History exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Encode
import Page.About
import Page.History
import Page.Home
import Page.NotFound
import Route
import Session exposing (Session)
import Url



-- MODEL


type Model
    = Home Page.Home.Model
    | History Page.History.Model
    | NotFound Page.NotFound.Model
    | About Page.About.Model


init : Json.Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            Page.Home.init (Session url key)
                |> updateWith Home GotHomeMsg
                |> Tuple.first
    in
    changeRouteTo (Route.fromUrl url) model



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg subView =
            let
                { title, body } =
                    subView
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Home subModel ->
            viewPage GotHomeMsg (Page.Home.view subModel)

        History subModel ->
            viewPage GotHistoryMsg (Page.History.view subModel)

        NotFound subModel ->
            viewPage GotNotFoundMsg (Page.NotFound.view subModel)

        About subModel ->
            viewPage GotAboutMsg (Page.About.view subModel)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Page.Home.Msg
    | GotHistoryMsg Page.History.Msg
    | GotNotFoundMsg Page.NotFound.Msg
    | GotAboutMsg Page.About.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Page.Home.update subMsg subModel
                |> updateWith Home GotHomeMsg

        ( GotHistoryMsg subMsg, History subModel ) ->
            Page.History.update subMsg subModel
                |> updateWith History GotHistoryMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            Page.NotFound.update subMsg subModel
                |> updateWith NotFound GotNotFoundMsg

        ( GotAboutMsg subMsg, About subModel ) ->
            Page.About.update subMsg subModel
                |> updateWith About GotAboutMsg

        ( _, _ ) ->
            -- Descarta as mensagens que foram enviadas para a página errada
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            Sub.map GotHomeMsg <| Page.Home.subscriptions subModel

        History subModel ->
            Sub.map GotHistoryMsg <| Page.History.subscriptions subModel

        NotFound subModel ->
            Sub.map GotNotFoundMsg <| Page.NotFound.subscriptions subModel

        About subModel ->
            Sub.map GotAboutMsg <| Page.About.subscriptions subModel



-- MAIN


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



-- INTERNAL


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession model =
    case model of
        Home subModel ->
            Page.Home.toSession subModel

        History subModel ->
            Page.History.toSession subModel

        NotFound subModel ->
            Page.NotFound.toSession subModel

        About subModel ->
            Page.About.toSession subModel


changeRouteTo : Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Route.Home ->
            Page.Home.init session
                |> updateWith Home GotHomeMsg

        Route.History from to ->
            Page.History.init session from to
                |> updateWith History GotHistoryMsg

        Route.NotFound ->
            Page.NotFound.init session
                |> updateWith NotFound GotNotFoundMsg

        Route.About ->
            Page.About.init session
                |> updateWith About GotAboutMsg

