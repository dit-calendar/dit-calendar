module Page.Register exposing (Model, RegisterModel, Msg(..), init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser
import Endpoint.ResponseErrorDecoder exposing (registerErrorDecoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Body, Expect)
import Http.Detailed as HttpEx
import Json.Encode as Encode
import Tuple exposing (mapFirst)


type alias RegisterModel =
    { name : String
    , email : String
    , password : String
    , passwordConfirm : String
    }


type alias Model =
    { register : RegisterModel
    , problems : List String
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (RegisterModel "" "" "" "") [], Cmd.none )


type UserMsg
    = Name String
    | Email String
    | Password String
    | PasswordConfirm String


type Msg
    = RegisterUserMsg UserMsg
    | PerformRegister
    | RegisterResult (Result (HttpEx.Error String) ( Http.Metadata, String ))


type alias RegisterResponse =
    { jrStatus : String
    , jrData : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterUserMsg rMsg ->
            mapFirst (\x -> { model | register = x }) (updateRegister rMsg model.register)

        PerformRegister ->
            ( model, register model )

        RegisterResult result ->
            ( registerResponse result model, Cmd.none )


updateRegister : UserMsg -> RegisterModel -> ( RegisterModel, Cmd Msg )
updateRegister msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordConfirm password ->
            ( { model | passwordConfirm = password }, Cmd.none )


registerResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
registerResponse response model =
    case response of
        Ok value ->
            model

        Err error ->
            { model | problems = List.append model.problems (registerErrorDecoder error) }


view : Model -> Html Msg
view model =
    div []
        [ viewRegister model.register
        , div []
            [ button [ onClick PerformRegister ]
                [ text "Submit" ]
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]

viewRegister : RegisterModel -> Html Msg
viewRegister model =
    div [class "register-fields"]
            [ viewInput "text" "Name" model.name (RegisterUserMsg << Name)
            , viewInput "text" "Email" model.email (RegisterUserMsg << Email)
            , viewInput "password" "Password" model.password (RegisterUserMsg << Password)
            , viewInput "password" "Repeat Password" model.passwordConfirm (RegisterUserMsg << PasswordConfirm)
            ]

viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


register : Model -> Cmd Msg
register model =
    Http.post
        { url = "https://localhost:8443/authenticate/authentication-methods/password/account"
        , body = Http.jsonBody (registerEncoder model)
        , expect = HttpEx.expectString RegisterResult
        }


registerEncoder : Model -> Encode.Value
registerEncoder model =
    let
        naUser =
            Encode.object
                [ ( "email", Encode.string model.register.email )
                , ( "username", Encode.string model.register.name )
                , ( "userId", Encode.int 0 )
                ]
    in
    Encode.object
        [ ( "naUser", naUser )
        , ( "naPassword", Encode.string model.register.password )
        , ( "naPasswordConfirm", Encode.string model.register.passwordConfirm )
        ]
