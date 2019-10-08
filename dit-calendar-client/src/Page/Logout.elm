module Page.Logout exposing (update)

import Browser.Navigation as Navigation
import Data.Logout exposing (Msg(..))
import Endpoint.AuthEndpoint as AuthEndpoint


update : Msg -> Cmd Msg
update msg =
    case msg of
        TriggerLogout ->
            AuthEndpoint.logout

        HttpLogout result ->
            case result of
                Ok _ ->
                    Navigation.load "#login"

                Err error ->
                    Cmd.none
