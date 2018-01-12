{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Exception     ( finally )
import Control.Monad.Reader  ( runReaderT )
import Data.Acid             ( AcidState )

import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site, setDefault, RouteT )

import Happstack.Server                       ( ServerPartT, mapServerPartT, nullConf
                                              , simpleHTTP, Response )
import Happstack.Authenticate.Core            ( AuthenticateURL(..), AuthenticateState )
import Happstack.Authenticate.Route           ( initAuthentication )
import Happstack.Authenticate.Password.Route  ( initPassword )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Auth.Auth                    ( authOrRoute, passwordConfig, authenticateConfig )
import Route.PageEnum               ( Sitemap(Home), urlSitemapParser )


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (`runReaderT` acid) sp

site :: AcidState AuthenticateState
       -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site authenticateState routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (authOrRoute authenticateState routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run =
  do (cleanup, routeAuthenticate, authenticateState) <-
        initAuthentication Nothing authenticateConfig
          [ initPassword passwordConfig ]
     withAcid Nothing
      (\ acid ->
         let appWithRoutetSite = implSite "http://localhost:8000" ""
              (site authenticateState routeAuthenticate)
            in simpleHTTP nullConf $ runApp acid appWithRoutetSite)
      `finally` cleanup
