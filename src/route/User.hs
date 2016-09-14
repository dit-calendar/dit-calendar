module Route.User where

import Domain.User

import Prelude                 hiding (head)

import Control.Monad           (msum)
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Data.Text
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes
    ( PathInfo(..), RouteT, showURL
    , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)

--function that maps a route to the handlers:
route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home          -> homePage
      (User userId) -> userPage userId

--handler for homePage
homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  ok $ toResponse "bar"

--handler for userPage
userPage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
userPage (UserId userId) = do
  ok $ toResponse "foo"

--does the routing?
site :: Site Sitemap (ServerPartT IO Response)
site =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT route in
  --convert the new function to a Site
  let realSite = mkSitePI realRoute in
       setDefault Home realSite