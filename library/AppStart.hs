{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Exception     ( finally )
import Control.Monad.Reader  ( runReaderT )
import Data.Acid             ( AcidState )

import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site, setDefault, RouteT )

import Happstack.Server                       ( ServerPartT, mapServerPartT, nullConf, Response )
import Happstack.Server.SimpleHTTPS           ( simpleHTTPS, TLSConf(..), nullTLSConf  )
import Happstack.Authenticate.Core            ( AuthenticateURL(..), AuthenticateState, usernamePolicy
                                              , AuthenticateConfig(..) )
import Happstack.Authenticate.Password.Core   ( PasswordConfig(..) )
import Happstack.Authenticate.Route           ( initAuthentication )
import Happstack.Authenticate.Password.Route  ( initPassword )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Auth.Authorization           ( authOrRoute )
import Route.PageEnum               ( Sitemap(Home), urlSitemapParser )

import qualified Data.Text as T


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

authenticateConfig :: AuthenticateConfig
authenticateConfig = AuthenticateConfig
             { _isAuthAdmin        = const $ return True
             , _usernameAcceptable = usernamePolicy
             , _requireEmail       = True
             }

passwordConfig :: PasswordConfig
passwordConfig = PasswordConfig
             { _resetLink = T.pack "https://localhost:8443/#resetPassword"
             , _domain    = T.pack "example.org"
             , _passwordAcceptable = \t ->
                 if T.length t >= 5
                 then Nothing
                 else Just $ T.pack "Must be at least 5 characters."
             }

tlsConf :: TLSConf
tlsConf =
    nullTLSConf { tlsPort = 8443
                , tlsCert = "Auth/ssl/dummy.localhost.crt"
                , tlsKey  = "Auth/ssl/dummy.privatelocalhost.key"
                }

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run =
  do (cleanup, routeAuthenticate, authenticateState) <-
        initAuthentication Nothing authenticateConfig
          [ initPassword passwordConfig ]
     withAcid Nothing
      (\ acid ->
         let appWithRoutetSite = implSite "https://localhost:8443" ""
              (site authenticateState routeAuthenticate)
            in simpleHTTPS tlsConf $ runApp acid appWithRoutetSite)
      `finally` cleanup
