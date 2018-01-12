{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Exception     ( finally )
import Control.Monad.Reader  ( runReaderT )
import Data.Acid             ( AcidState )

import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site(..), setDefault, RouteT(..) )

import Happstack.Server      ( ServerPartT(..), mapServerPartT, nullConf, simpleHTTP
                             , Response, ServerPartT )
import Happstack.Authenticate.Core ( AuthenticateURL(..), AuthenticateConfig(..), AuthenticateState, usernamePolicy )
import Happstack.Authenticate.Route ( initAuthentication )
import Happstack.Authenticate.Password.Core( PasswordConfig(..) )
import Happstack.Authenticate.Password.Route ( initPassword )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Route.Routing                ( authThenRoute )
import Route.PageEnum               ( Sitemap(..), urlSitemapParser )

import qualified Data.Text as T


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (`runReaderT` acid) sp

site :: AcidState AuthenticateState
       -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site authenticateState routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (authThenRoute authenticateState routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

authenticateConfig :: AuthenticateConfig
authenticateConfig = AuthenticateConfig
             { _isAuthAdmin        = const $ return True
             , _usernameAcceptable = usernamePolicy
             , _requireEmail       = True
             }

passwordConfig :: PasswordConfig
passwordConfig = PasswordConfig
             { _resetLink = "http://localhost:8000/#resetPassword"
             , _domain    =  "example.org"
             , _passwordAcceptable = \t ->
                 if T.length t >= 5
                 then Nothing
                 else Just "Must be at least 5 characters."
             }

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
