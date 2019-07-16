module Page.Register exposing (Model, Msg(..), RegisterModel, init, update, view, viewInput)

import Bootstrap.Alert as Alert
import Browser
import Endpoint.ResponseErrorDecoder exposing (authErrorDecoder)
import Env.Serverurl as Server
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


type RegisterMsg
    = Name String
    | Email String
    | Password String
    | PasswordConfirm String


type Msg
    = RegisterUserMsg RegisterMsg
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


updateRegister : RegisterMsg -> RegisterModel -> ( RegisterModel, Cmd Msg )
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
            { model | problems = authErrorDecoder error }


view : Model -> Html Msg
view model =
    div []
        [ Html.map RegisterUserMsg (viewRegister model.register)
        , div []
            [ button [ onClick PerformRegister ]
                [ text "Submit" ]
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewRegister : RegisterModel -> Html RegisterMsg
viewRegister model =
    div [ class "register-fields" ]
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Email" model.email Email
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Repeat Password" model.passwordConfirm PasswordConfirm
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
        { url = Server.registerUrl
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
