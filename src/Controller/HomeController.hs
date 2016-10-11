{-# LANGUAGE TemplateHaskell #-}

module Controller.HomeController where

import Route.PageEnum
import Domain.User

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Web.Routes.Happstack    ( implSite )
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )
import Web.Routes.TH  ( derivePathInfo )

--handler for homePage
getHomePage :: AcidState UserState -> RouteT SiteMap (ServerPartT IO) Response
getHomePage acid =
  do
       c <- query' acid PeekName
       ok $ toResponse $ "peeked at the name and saw: " ++ show c
