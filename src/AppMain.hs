--Start-Module darf nicht anders als Main heißen
module Main where

import Domain.User

import Control.Monad           (msum)
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)

import Web.Routes
    ( PathInfo(..), RouteT, showURL
    , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
        Home          -> homePage
        (User userId) -> userPage userId

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  ok $ toResponse "Home\n"

userPage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
userPage (UserId i) = do
    ok $ toResponse "You did it.\n"


main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "foo" $ notFound (toResponse ())
  , seeOther "/route" (toResponse ())
  ]
