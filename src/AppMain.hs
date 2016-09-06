--Start-Module darf nicht anders als Main heißen
module Main where

import Domain.User
import Route.User

import Prelude                 hiding (head)

import Control.Monad           (msum)
import Data.Text
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes.Happstack    (implSite)

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite (pack "http://localhost:8000") (pack "/route") site
  , seeOther "/r" (toResponse ())
  ]
