module Data.Logout exposing (Msg(..))

import Http
import Http.Detailed as HttpEx

type Msg
    = TriggerLogout
    | HttpLogout (Result (HttpEx.Error String) ( Http.Metadata, String ))