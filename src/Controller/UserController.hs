{-# LANGUAGE TemplateHaskell #-}

module Controller.UserController where

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

--handler for userPage
getUserPage :: RouteT SiteMap (ServerPartT IO) Response
getUserPage = do
  ok $ toResponse "foo"

