{-# LANGUAGE TemplateHaskell #-}

module Controller.UserController where

import Route.PageEnum

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
getUserPage :: Integer -> RouteT SiteMap (ServerPartT IO) Response
getUserPage i  = do
  ok $ toResponse ("foo" ++ show i)

