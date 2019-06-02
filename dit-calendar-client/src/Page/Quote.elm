module Page.Quote exposing (Model, Msg(..), emptyModel, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
    { quote : String
    }


emptyModel : Model
emptyModel =
    { quote = "" }


init : ( Model, Cmd Msg )
init  =
    (emptyModel , Cmd.none )


type Msg
    = GetQuote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( { model | quote = model.quote ++ "A quote! " }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick GetQuote ] [ text "Grab a quote!" ]
            ]

        -- Blockquote with quote
        , blockquote []
            [ p [] [ text model.quote ]
            ]
        ]