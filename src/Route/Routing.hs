{-# LANGUAGE TemplateHaskell #-}

module Route.Routing where

import Domain.User
import Controller.UserController
import Controller.HomeController
import Route.Url

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Web.Routes.Happstack    ( implSite )
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )
import Web.Routes.TH  ( derivePathInfo )

--function that maps a route to the handlers:
route :: AcidState UserState -> SiteMap -> RouteT SiteMap (ServerPartT IO) Response
route acid url =
    case url of
      HomePage       -> getHomePage acid
      UserPage       -> getUserPage

--does the routing?
site :: AcidState UserState -> Site SiteMap (ServerPartT IO Response)
site acid =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT $ route acid in
  --convert the new function to a Site
  let realSite = mkSitePI realRoute in
       setDefault HomePage realSite
