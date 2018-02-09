{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Exception     ( finally )
import Control.Monad.Reader  ( runReaderT )
import Data.Acid             ( AcidState )

import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site, setDefault, RouteT )

import Happstack.Server                       ( ServerPartT, mapServerPartT, Response )
import Happstack.Server.SimpleHTTPS           ( simpleHTTPS )
import Happstack.Authenticate.Core            ( AuthenticateURL(..), AuthenticateState )
import Happstack.Authenticate.Route           ( initAuthentication )
import Happstack.Authenticate.Password.Route  ( initPassword )

import Controller.AcidHelper        ( withAcid, Acid, App )
import Auth.Authorization           ( authOrRoute, authenticateConfig, passwordConfig, tlsConf )
import Route.PageEnum               ( Sitemap(Home), urlSitemapParser )

import qualified Data.Text as T


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid = mapServerPartT (`runReaderT` acid)

site :: AcidState AuthenticateState
       -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site authenticateState routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (authOrRoute authenticateState routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter https://localhost:8443
run :: IO ()
run =
  do (cleanup, routeAuthenticate, authenticateState) <-
        initAuthentication Nothing authenticateConfig
          [ initPassword passwordConfig ]
     withAcid Nothing
      (\ acid ->
         let appWithRoutetSite = implSite "https://localhost:8000" ""
              (site authenticateState routeAuthenticate)
            in simpleHTTPS tlsConf $ runApp acid appWithRoutetSite)
      `finally` cleanup
