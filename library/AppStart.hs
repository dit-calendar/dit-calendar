module AppStart where

import Control.Monad.Reader  ( runReaderT )
import Happstack.Server      ( ServerPartT(..), mapServerPartT, nullConf, simpleHTTP
                             , Response, ServerPartT )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site(..), setDefault, mkSitePI)
import Data.Text             ( pack )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Route.Routing                ( route )
import Route.PageEnum               ( SiteMap(..) )


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) =
    mapServerPartT (`runReaderT` acid) sp

site :: Site SiteMap (App Response)
site =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT route in
  --convert the new function to a Site
  let realSite = mkSitePI realRoute in
        setDefault Home realSite

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run =
    withAcid Nothing $ \acid ->
        let appWithRoutetSite = implSite (pack "http://localhost:8000") (pack "/route") site in
            simpleHTTP nullConf $ runApp acid appWithRoutetSite
