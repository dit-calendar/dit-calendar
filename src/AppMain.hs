--Start-Module darf nicht anders als Main heißen
module Main where

import Domain.User

import Control.Monad           (msum)
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "foo" $ notFound (toResponse ())
  , seeOther "/route" (toResponse ())
  ]
