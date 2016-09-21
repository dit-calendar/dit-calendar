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
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )

--function that maps a route to the handlers:
route :: AcidState UserState -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url =
    case url of
      Home          -> homePage acid
      (User userId) -> userPage userId

--handler for homePage
homePage :: AcidState UserState -> RouteT Sitemap (ServerPartT IO) Response
homePage acid = 
  do
       c <- query' acid PeekName
       ok $ toResponse $"peeked at the name and saw: " ++ show c

--handler for userPage
userPage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
userPage (UserId userId) = do
  ok $ toResponse "foo"

--does the routing?
site :: AcidState UserState -> Site Sitemap (ServerPartT IO Response)
site acid =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT $ route acid in
  --convert the new function to a Site
  let realSite = mkSitePI realRoute in
       setDefault Home realSite
