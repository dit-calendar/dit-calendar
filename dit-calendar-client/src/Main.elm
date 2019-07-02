module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CalendarEntryDetails as CalendarEntryDetails
import Page.Login as Login
import Page.Register as Register exposing (RegisterModel)
import Page.SimpleCalendar as Calendar
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)


type alias Flags =
    { authUrl : String
    }


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , config : Flags
    }


type Page
    = Login Login.Model
    | Register Register.Model
    | SimpleCalendar Calendar.Model
    | CalendarDetails CalendarEntryDetails.Model
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
            urlUpdate url { navKey = key, navState = navState, page = Login { name = "", password = "", problems = [] }, config = flags }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | CalendarMsg Calendar.Msg
    | CalendarDetailMsg CalendarEntryDetails.Msg


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

        LoginMsg loginMsg ->
            case model.page of
                Login login ->
                    stepLogin model (Login.update model.config.authUrl loginMsg login)

                _ ->
                    --TODO kann das enfernt werden?
                    ( model, Cmd.none )

        RegisterMsg regMsg ->
            case model.page of
                Register register ->
                    stepRegister model (Register.update model.config.authUrl regMsg register)

                _ ->
                    ( model, Cmd.none )

        CalendarMsg calendarMsg ->
            case calendarMsg of
                Calendar.OpenCalendarDetialsView entry ->
                    stepCalendarDetails model (CalendarEntryDetails.init entry)

                _ ->
                    case model.page of
                        SimpleCalendar calendar ->
                            stepCalendar model (Calendar.update calendarMsg calendar)

                        _ ->
                            ( model, Cmd.none )

        CalendarDetailMsg calendarDetailMsg ->
            case model.page of
                CalendarDetails calendarDetails ->
                    stepCalendarDetails model (CalendarEntryDetails.update calendarDetailMsg calendarDetails)

                _ ->
                    ( model, Cmd.none )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( login, cmds ) =
    ( { model | page = Login login }
    , Cmd.map LoginMsg cmds
    )


stepRegister : Model -> ( Register.Model, Cmd Register.Msg ) -> ( Model, Cmd Msg )
stepRegister model ( register, cmds ) =
    ( { model | page = Register register }
    , Cmd.map RegisterMsg cmds
    )


stepCalendar : Model -> ( Calendar.Model, Cmd Calendar.Msg ) -> ( Model, Cmd Msg )
stepCalendar model ( calendar, cmds ) =
    ( { model | page = SimpleCalendar calendar }
    , Cmd.map CalendarMsg cmds
    )


stepCalendarDetails : Model -> ( CalendarEntryDetails.Model, Cmd CalendarEntryDetails.Msg ) -> ( Model, Cmd Msg )
stepCalendarDetails model ( calendarDetail, cmds ) =
    ( { model | page = CalendarDetails calendarDetail }
    , Cmd.map CalendarDetailMsg cmds
    )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            case route of
                SimpleCalendar _ ->
                    -- needed to perform request if url was changed
                    stepCalendar model (Calendar.update Calendar.PerformGetCalendarEntries Calendar.emptyModel)

                _ ->
                    ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    let
        reg =
            RegisterModel "" "" "" ""
    in
    UrlParser.oneOf
        [ UrlParser.map (Login { name = "", password = "", problems = [] }) top
        , UrlParser.map (Login { name = "", password = "", problems = [] }) (s "login")
        , UrlParser.map (Register { register = reg, problems = [] }) (s "register")
        , UrlParser.map (SimpleCalendar Calendar.emptyModel) (s "calendar")
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
            , Navbar.itemLink [ href "#register" ] [ text "Register" ]
            , Navbar.itemLink [ href "#calendar" ] [ text "Calendar" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Login login ->
                [ Html.map LoginMsg (Login.view login) ]

            Register register ->
                [ Html.map RegisterMsg (Register.view register) ]

            NotFound ->
                pageNotFound

            SimpleCalendar calendars ->
                [ Html.map CalendarMsg (Calendar.view calendars) ]

            CalendarDetails calendarDetail ->
                [ Html.map CalendarDetailMsg (CalendarEntryDetails.view calendarDetail) ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]
