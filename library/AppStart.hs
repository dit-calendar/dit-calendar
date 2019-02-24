{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import           Prelude                               hiding (readFile)

import           Control.Exception                     (finally)
import           Control.Monad.Reader                  (runReaderT)
import           Data.Text.IO                          (readFile)

import           Web.Routes                            (RouteT, Site, runRouteT,
                                                        setDefault)
import           Web.Routes.Boomerang                  (boomerangSite)
import           Web.Routes.Happstack                  (implSite)

import           Happstack.Authenticate.Core           (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Route (initPassword)
import           Happstack.Authenticate.Route          (initAuthentication)
import           Happstack.Server                      (Response, ServerPartT,
                                                        mapServerPartT)
import           Happstack.Server.SimpleHTTPS          (simpleHTTPS)

import           AcidHelper                            (Acid, App, withAcid)
import           Conf.AuthConf                         (authenticateConfig,
                                                        passwordConfig, tlsConf)
import           Conf.Config
import           Presentation.Route.PageEnum           (Sitemap (Home),
                                                        urlSitemapParser)
import           Presentation.Route.Routing            (authOrRoute)

import qualified Data.Text                             as T


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid = mapServerPartT (`runReaderT` acid)

site :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (authOrRoute routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter https://localhost:8443
run :: IO ()
run = do
    textConfig <- readFile "application.cfg"
    case readConfig textConfig of
        Left error -> print $ "error with config file: " ++ error
        Right conf -> do
            putStrLn $ "Server unter: " ++ hostUri (cfNetwork conf)
            (cleanup, routeAuthenticate, authenticateState) <-
                initAuthentication Nothing authenticateConfig [ initPassword (passwordConfig conf) ]
            let startServer acid = simpleHTTPS (tlsConf conf) $ runApp acid appWithRoutetSite
                appWithRoutetSite = implSite (T.pack $ hostUri (cfNetwork conf)) "" (site routeAuthenticate) in
                withAcid authenticateState Nothing startServer `finally` cleanup
