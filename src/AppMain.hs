--Start-Module darf nicht anders als Main heißen
module Main where

import Domain.User as Domain
import Route.Routing as Route

import Prelude                 hiding ( head )

import Control.Monad           (msum)
import Data.Text
import Happstack.Server
    ( toResponse, simpleHTTP, nullConf,
    seeOther, dir, notFound, seeOther)
import Web.Routes.Happstack    ( implSite )
import Control.Exception    ( bracket )
import Data.Acid            ( openLocalState )
import Data.Acid.Local      ( createCheckpointAndClose )

main :: IO () 
main = 
    -- starts up acid-state. If no pre-existing state is found, then initialCounterState will be used 
    let currentState = openLocalState Domain.initialUserState in
        bracket currentState
            createCheckpointAndClose
            (\acid ->
                simpleHTTP nullConf $ msum
                    [ dir "favicon.ico" $ notFound (toResponse ())
                    , implSite (pack "http://localhost:8000") (pack "/route") $ Route.site acid
                    , seeOther "/r" (toResponse ())
                    ])