module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing

import Page.Login as Login
import Page.Quote as Quote


type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    }

type Page
    = Login Login.Model
    | Quote Quote.Model
    | NotFound

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Login { name = "", password = "" }}
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | LoginMsg Login.Msg
    | QuoteMsg Quote.Msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                 Browser.External href ->
                     ( model, Navigation.load href )
        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )
        LoginMsg quote ->
            case model.page of
                Login login ->
                    stepLogin model (Login.update quote login)
                _ ->
                    ( model, Cmd.none )

        QuoteMsg login ->
            case model.page of
                Quote quote ->
                    stepQuote model (Quote.update login quote)
                _ ->
                    ( model, Cmd.none )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( login, cmds ) =
    ( { model | page = Login login }
    , Cmd.map LoginMsg cmds
    )


stepQuote : Model -> ( Quote.Model, Cmd Quote.Msg ) -> ( Model, Cmd Msg )
stepQuote model ( quote, cmds ) =
    ( { model | page = Quote quote }
    , Cmd.map QuoteMsg cmds
    )




urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map (Login { name = "", password = "" }) top
        , UrlParser.map (Login { name = "", password = "" }) (s "login")
        , UrlParser.map (Quote { quote = "" }) (s "quotes")
        ]

view : Model -> Browser.Document Msg
view model =
    { title = "Elm Bootstrap"
    , body =
        [ div []
            [ menuView model
            , mainContent model
            ]
        ]
    }



menuView : Model -> Html Msg
menuView model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "#" ] [ text "Elm Bootstrap" ]
        |> Navbar.info
        |> Navbar.items
            [ Navbar.itemLink [ href "#login" ] [ text "Login" ]
            , Navbar.itemLink [ Spacing.ml2Sm,  href "#quotes" ] [ text "Quote" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Login login ->
                [skeletonView "URL Interceptor" LoginMsg (Login.view login)]

            Quote quote ->
                [skeletonView "URL Interceptor" QuoteMsg (Quote.view quote)]

            NotFound ->
                pageNotFound

skeletonView : String -> (a-> Msg) -> Html a -> Html Msg
skeletonView t message details =
    Html.map message details

pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]