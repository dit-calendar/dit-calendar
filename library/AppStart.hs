{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Monad.Reader  ( runReaderT )
import Happstack.Server      ( ServerPartT(..), mapServerPartT, nullConf, simpleHTTP
                             , Response, ServerPartT )
import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site(..), setDefault )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Route.Routing                ( route )
import Route.PageEnum               ( Sitemap(..), urlSitemapParser )


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (`runReaderT` acid) sp

site :: Site Sitemap (App Response)
site =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT route in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run = withAcid Nothing $ \acid ->
    let appWithRoutetSite = implSite "http://localhost:8000" "" site in
        simpleHTTP nullConf $ runApp acid appWithRoutetSite
