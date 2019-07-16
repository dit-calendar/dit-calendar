module Data.Login exposing (Model, Msg(..))

import Http
import Http.Detailed as HttpEx


type alias Model =
    { name : String
    , password : String
    , problems : List String
    }


type Msg
    = Name String
    | Password String
    | Login
    | HttpLogin (Result (HttpEx.Error String) ( Http.Metadata, String ))
