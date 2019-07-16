module Data.Register exposing (Model, Msg(..), RegisterModel, RegisterMsg(..))

import Http
import Http.Detailed as HttpEx


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


type RegisterMsg
    = Name String
    | Email String
    | Password String
    | PasswordConfirm String


type Msg
    = RegisterUserMsg RegisterMsg
    | PerformRegister
    | RegisterResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
