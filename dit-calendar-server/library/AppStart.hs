{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import           Prelude                               hiding (readFile)

import           Control.Exception                     (finally)
import           Data.Text.IO                          (readFile)
import           System.Environment                    (lookupEnv)

import           Web.Routes                            (RouteT, Site, runRouteT,
                                                        setDefault)
import           Web.Routes.Boomerang                  (boomerangSite)
import           Web.Routes.Happstack                  (implSite)

import           Happstack.Authenticate.Core           (AuthenticateURL (..))
import           Happstack.Authenticate.Password.Route (initPassword)
import           Happstack.Authenticate.Route          (initAuthentication)
import           Happstack.Server                      (Response, ServerPartT,
                                                        simpleHTTP)

import           AppContext                            (App, AppReader (..))
import           Conf.AuthConf                         (authenticateConfig,
                                                        passwordConfig)
import           Conf.Config                           (Config (..), readConfig)
import           Conf.NetworkConfig                    (customHappstackServerConf,
                                                        hostUri)
import           Data.Repository.Acid.DBState          (Acid)
import           Presentation.Route.MainRouting        (routeWithOptions)
import           Presentation.Route.PageEnum           (Sitemap (Home),
                                                        urlSitemapParser)
import           Server.AcidInitializer                (withAcid)
import           Server.HappstackHelper                (runServerWithFoundationT)

import qualified Data.Text                             as T

runApp :: App a -> Config -> Acid -> ServerPartT IO a
runApp app conf acid = runServerWithFoundationT app (AppReader acid conf Nothing)

site :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (routeWithOptions routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

bootServer :: Config -> IO ()
bootServer conf = do
    putStrLn $ "Server unter: " ++ hostUrl
    (cleanup, routeAuthenticate, authenticateState) <- initAuthentication Nothing authenticateConfig [ passwordConf ]
    let startServer acid = simpleHTTP serverConf $ runApp appWithRoutetSite conf acid
        appWithRoutetSite = implSite (T.pack hostUrl) "" (site routeAuthenticate) in
            withAcid authenticateState Nothing startServer `finally` cleanup
    where
        netWorkConf = cfNetwork conf
        hostUrl = hostUri netWorkConf
        serverConf = customHappstackServerConf netWorkConf
        passwordConf = initPassword (passwordConfig conf)

--zu HomePage zu erreichen unter http://localhost:8080
run :: IO ()
run = do
    textConfig <- readFile "application.cfg"
    portResult <- lookupEnv "PORT"
    case readConfig portResult textConfig of
        Left error -> putStrLn $ "error with reading config file: " ++ error
        Right conf -> bootServer conf
