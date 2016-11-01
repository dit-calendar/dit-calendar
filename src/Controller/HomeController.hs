{-# LANGUAGE TemplateHaskell #-}

module Controller.HomeController where

import Route.PageEnum as Page
import Domain.User as User

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse, lookRead )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Web.Routes.Happstack    ( implSite )
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )

--handler for homePage
getHomePage :: AcidState User.UserList -> RouteT Page.SiteMap (ServerPartT IO) Response
getHomePage acid =
    do mUser <- query' acid (UserById 1)
       case mUser of
         Nothing ->
             ok $ toResponse $ "Could not find a user with id " ++ show 1
         (Just u) ->
             ok $ toResponse $ "peeked at the name and saw: " ++ show (User.userId u)