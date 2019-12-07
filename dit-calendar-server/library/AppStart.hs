{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import           Prelude                               hiding (readFile)

import           Control.Exception                     (finally)
import           Data.Text.IO                          (readFile)

import           Web.Routes                            (RouteT, Site, runRouteT,
                                                        setDefault)
import           Web.Routes.Boomerang                  (boomerangSite)
import           Web.Routes.Happstack                  (implSite)

import           Happstack.Authenticate.Core           (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Route (initPassword)
import           Happstack.Authenticate.Route          (initAuthentication)
import           Happstack.Server                      (Response, ServerPartT,
                                                        nullConf, simpleHTTP)
import           Happstack.Server.Types                (Conf, port)

import           AcidHelper                            (Acid, App, withAcid, AppContext(..))
import           Conf.AuthConf                         (authenticateConfig,
                                                        passwordConfig)
import           Conf.Config
import           HappstackHelper                       (runServerWithFoundationT)
import           Presentation.Route.MainRouting        (routeWithOptions)
import           Presentation.Route.PageEnum           (Sitemap (Home),
                                                        urlSitemapParser)

import qualified Data.Text                             as T

initialReqSt = ()

runApp :: App a -> Acid -> ServerPartT IO a
runApp app acid = runServerWithFoundationT app initialReqSt (AppContext acid Nothing)

site :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (routeWithOptions routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter http://localhost:8443
run :: IO ()
run = do
    textConfig <- readFile "application.cfg"
    case readConfig textConfig of
        Left error -> print $ "error with config file: " ++ error
        Right conf -> do
            putStrLn $ "Server unter: " ++ hostUrl
            (cleanup, routeAuthenticate, authenticateState) <-
                initAuthentication Nothing authenticateConfig [ initPassword (passwordConfig conf) ]
            let startServer acid = simpleHTTP (customServerConf netWorkConf) $ runApp appWithRoutetSite acid
                appWithRoutetSite = implSite (T.pack hostUrl) "" (site routeAuthenticate) in
                withAcid authenticateState Nothing startServer `finally` cleanup
            where
                netWorkConf = cfNetwork conf
                hostUrl = hostUri netWorkConf

customServerConf :: NetworkConfig -> Conf
customServerConf netConf =
    nullConf { port = netPort netConf
             }
