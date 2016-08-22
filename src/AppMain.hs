--Start-Module darf nicht anders als Main heißen
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    TemplateHaskell #-}
module Main where

--import Domain.User

import Prelude                 hiding (head)

import Control.Monad           (msum)
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Data.Text
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes
    ( PathInfo(..), RouteT, showURL
    , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)


newtype UserId = UserId { unUserId :: Int }
    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
  = Home
  | User UserId
  
$(derivePathInfo ''Sitemap)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home                -> homePage
      (User userId) -> userPage userId

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  ok $ toResponse "bar"

userPage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
userPage (UserId userId) = do
  ok $ toResponse "foo"

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ mkSitePI (runRouteT route)

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite (pack "http://localhost:8000") (pack "/route") site
  , seeOther "/r" (toResponse ())
  ]

-- handler :: Sitemap -> RouteT Sitemap IO ()
-- handler route = case route of
--   Index -> do
--     posts <- liftIO getPosts
--     liftIO $ putStrLn "Posts:"
--     forM_ posts $ \post -> do
--       postUrl <- showURL (Post (postId post))
--       liftIO $ putStrLn $
--         Text.unpack (postTitle post) ++ " - " ++ Text.unpack postUrl

