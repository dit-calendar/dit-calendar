--Start-Module darf nicht anders als Main heißen
module Main where

import Control.Monad    (msum)
import Happstack.Server ( Method(GET, POST), dir, method
                        , nullConf, ok, simpleHTTP, seeOther
                        )

main :: IO ()
main = simpleHTTP nullConf $ msum
       [ do dir "foo" $ do method GET
                           ok $ "You did a GET request on /foo\n"
       , do method GET
            ok $ "You did a GET request.\n"
       , do method POST
            ok $ "You did a POST request.\n"
       ]

foo :: () -- ^ The unit type.
foo = ()