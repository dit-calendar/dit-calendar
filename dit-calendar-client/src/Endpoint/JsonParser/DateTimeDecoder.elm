module Endpoint.JsonParser.DateTimeDecoder exposing (stringToDate, stringToDateTime)

import Json.Decode as Decode exposing (Decoder, Value, andThen, succeed)
import String exposing (left)
import String.Extra exposing (leftOf, rightOf)


stringToDate : Decoder String
stringToDate =
    Decode.string
        |> andThen (\val -> succeed <| leftOf "T" val)


stringToDateTime : Decoder String
stringToDateTime =
    Decode.string
        |> andThen (\val -> succeed <| left 5 (rightOf "T" val))
