module Page.SimpleCalendar exposing (CalendarEntry, Model, Msg(..), emptyModel, main, update, view)

import Bootstrap.ListGroup as ListGroup
import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)


type alias CalendarEntry =
    { description : String
    , date : String
    }


type alias Model =
    { calendarEntries : List CalendarEntry
    }


emptyModel : Model
emptyModel =
    { calendarEntries = [] }


type Msg
    = PerformGetCalendarEntries
    | GetCalendarEntriesResult (Result (HttpEx.Error String) ( Http.Metadata, String ))


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
            ( model, loadCalendarEntries model )

        GetCalendarEntriesResult result ->
            ( loadCalendarEntriesResponse result model, Cmd.none )


loadCalendarEntries : Model -> Cmd Msg
loadCalendarEntries model =
    Http.post
        { url = "https://localhost:8443/calendarentries/2"
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntriesResult
        }


loadCalendarEntriesResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
loadCalendarEntriesResponse response model =
    case response of
        Ok value ->
            { model | calendarEntries = calendarEntriesDecoder value }

        Err error ->
            --TODO beim decodieren des Fehlers ist was scheifgelaufen
            { model | calendarEntries = [ CalendarEntry "fehler beim request" "" ] }


calendarEntriesDecoder : ( Http.Metadata, String ) -> List CalendarEntry
calendarEntriesDecoder ( meta, body ) =
    let
        decode =
            Decode.decodeString calendarEntryDecoder body
    in
    case decode of
        Ok calendarEntry ->
            [ calendarEntry ]

        Err error ->
            --TODO beim decodieren des Fehlers ist was schiefgelaufen
            [ CalendarEntry "fehler beim decodieren" "" ]


calendarEntryDecoder : Decode.Decoder CalendarEntry
calendarEntryDecoder =
    Decode.map2
        CalendarEntry
        (Decode.at [ "description" ] Decode.string)
        (Decode.field "date" Decode.string)


view : Model -> Html Msg
view model =
    div [ class "calendar-entries" ]
        [ h1 [] [ text "Kalendar Einträge" ]
        , ListGroup.ul
            (List.map
                (\entry ->
                    ListGroup.li [] [ text ("description: " ++ entry.description ++ ", date:" ++ entry.date) ]
                )
                model.calendarEntries
            )
        ]
