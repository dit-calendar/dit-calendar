module Page.SimpleCalendarList exposing (Model, Msg(..), emptyModel, main, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.ListGroup as ListGroup
import Browser
import Data.CalendarEntry exposing (CalendarEntry)
import Endpoint.CalendarEntryEndpoint exposing (calendarErrorDecoder)
import Env.Serverurl as Server
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


type alias Model =
    { calendarEntries : List CalendarEntry
    , problems : List String
    }


emptyModel : Model
emptyModel =
    { calendarEntries = [], problems = [] }


type Msg
    = PerformGetCalendarEntries
    | GetCalendarEntriesResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenCalendarDetialsView CalendarEntry


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
    update PerformGetCalendarEntries emptyModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PerformGetCalendarEntries ->
            ( model, loadCalendarEntries )

        GetCalendarEntriesResult result ->
            ( loadCalendarEntriesResponse result model, Cmd.none )

        OpenCalendarDetialsView _ ->
            --TODO rais logic error exception
            ( model, Cmd.none )


loadCalendarEntries : Cmd Msg
loadCalendarEntries =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarEntries
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntriesResult
        , timeout = Nothing
        , tracker = Nothing
        }


loadCalendarEntriesResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
loadCalendarEntriesResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    calendarEntriesDecoder value
            in
            case resp of
                Ok calendarEntries ->
                    { model | calendarEntries = calendarEntries }

                Err error ->
                    { model | problems = [ error ] }

        Err error ->
            { model | problems = calendarErrorDecoder error }


calendarEntriesDecoder : ( Http.Metadata, String ) -> Result String (List CalendarEntry)
calendarEntriesDecoder ( meta, body ) =
    let
        decode =
            Decode.decodeString calendarEntryDecoder body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren des calendars" ++ Decode.errorToString error)


calendarEntryDecoder : Decode.Decoder (List CalendarEntry)
calendarEntryDecoder =
    Decode.list
        (Decode.map5
            CalendarEntry
            (Decode.nullable (Decode.field "entryId" Decode.int))
            (Decode.field "version" Decode.int)
            (Decode.at [ "description" ] Decode.string)
            (Decode.field "startDate" Decode.string)
            (Decode.field "endDate" Decode.string)
        )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "calendar-entries" ]
            [ h1 [] [ text "Kalendar Einträge" ]
            , ListGroup.custom
                (List.map
                    (\entry ->
                        ListGroup.button [ ListGroup.attrs [ onClick (OpenCalendarDetialsView entry) ] ] [ text ("description: " ++ entry.description ++ ", start date:" ++ entry.startDate) ]
                    )
                    model.calendarEntries
                )
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]
