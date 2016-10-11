{-# LANGUAGE TemplateHaskell #-}

module Route.User where

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

--A url type
data Sitemap
  = Home
  | User
$(derivePathInfo ''Sitemap)

--function that maps a route to the handlers:
route :: AcidState UserState -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url =
    case url of
      Home          -> homePage acid
      User          -> userPage

--handler for homePage
homePage :: AcidState UserState -> RouteT Sitemap (ServerPartT IO) Response
homePage acid =
  do
       c <- query' acid PeekName
       ok $ toResponse $ "peeked at the name and saw: " ++ show c

--handler for userPage
userPage :: RouteT Sitemap (ServerPartT IO) Response
userPage = do
  ok $ toResponse "foo"

--does the routing?
site :: AcidState UserState -> Site Sitemap (ServerPartT IO Response)
site acid =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT $ route acid in
  --convert the new function to a Site
  let realSite = mkSitePI realRoute in
       setDefault Home realSite
