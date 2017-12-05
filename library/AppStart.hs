{-# LANGUAGE OverloadedStrings #-}

module AppStart where

import Control.Exception (bracket, finally)
import Control.Monad.Reader  ( runReaderT )
import Happstack.Server      ( ServerPartT(..), mapServerPartT, nullConf, simpleHTTP
                             , Response, ServerPartT )
import Web.Routes.Boomerang  ( boomerangSite )
import Web.Routes.Happstack  ( implSite )
import Web.Routes            ( runRouteT, Site(..), setDefault, RouteT(..) )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Route.Routing                ( route )
import Route.PageEnum               ( Sitemap(..), urlSitemapParser )

import Data.Acid (AcidState)

import Happstack.Authenticate.Core (AuthenticateURL(..), AuthenticateConfig(..), AuthenticateState, Email(..), User(..), Username(..), UserId(..), GetAuthenticateState(..), decodeAndVerifyToken, tokenUser, usernamePolicy)
import Happstack.Authenticate.Route (initAuthentication)
import Happstack.Authenticate.Password.Controllers(usernamePasswordCtrl)
import Happstack.Authenticate.OpenId.Controllers(openIdCtrl)
import Happstack.Authenticate.Password.Core(PasswordConfig(..), PasswordState)
import Happstack.Authenticate.Password.Route (initPassword)
import Happstack.Authenticate.Password.URL(PasswordURL(..))
import Happstack.Authenticate.OpenId.Core  (OpenIdState)
import Happstack.Authenticate.OpenId.Route (initOpenId)
import Happstack.Authenticate.OpenId.URL (OpenIdURL(..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (`runReaderT` acid) sp

site ::AcidState AuthenticateState
       -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
       -> Site Sitemap (App Response)
site authenticateState routeAuthenticate =
  --runRouteT removes the RouteT wrapper from our routing function
  let realRoute = runRouteT (route authenticateState routeAuthenticate) in
  --convert the new function to a Site
  let realSite = boomerangSite realRoute urlSitemapParser in
        setDefault Home realSite

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run =
  do (cleanup, routeAuthenticate, authenticateState) <-
       let authenticateConfig = AuthenticateConfig
             { _isAuthAdmin        = const $ return True
             , _usernameAcceptable = usernamePolicy
             , _requireEmail       = True
             }
           passwordConfig = PasswordConfig
             { _resetLink = "http://localhost:8000/#resetPassword"
             , _domain    =  "example.org"
             , _passwordAcceptable = \t ->
                 if T.length t >= 5
                 then Nothing
                 else Just "Must be at least 5 characters."
             }
       in
         initAuthentication Nothing authenticateConfig
           [ initPassword passwordConfig ]
     (withAcid Nothing $ \acid ->
          let appWithRoutetSite = implSite "http://localhost:8000" "" (site authenticateState routeAuthenticate) in
              simpleHTTP nullConf $ runApp acid appWithRoutetSite) `finally` cleanup
