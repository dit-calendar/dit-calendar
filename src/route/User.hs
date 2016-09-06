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

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home                -> homePage
      (User userId) -> userPage userId

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  ok $ toResponse "bar"

userPage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
userPage (UserId userId) = do
  ok $ toResponse "foo"

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ mkSitePI (runRouteT route)
