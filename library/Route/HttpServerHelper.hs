module Route.HttpServerHelper ( getBody, readAuthUserFromBodyAsList, getHttpMethod ) where

import Control.Monad.IO.Class      ( liftIO, MonadIO )

import Happstack.Authenticate.Password.Core  ( NewAccountData )
import Happstack.Server.Types                ( RqBody, Request )
import Happstack.Server                      ( ok, Method, nullDir, unBody, rqBody
                                             , Request(rqMethod), askRq )
import Control.Concurrent.MVar               ( tryReadMVar )
import Data.Aeson                            ( decode )

import Web.Routes                            ( RouteT )
import Controller.AcidHelper                 ( App )
import Route.PageEnum                        ( Sitemap )

import qualified Data.ByteString.Lazy.Char8 as L

peekRequestBody :: (MonadIO m) => Request -> m (Maybe RqBody)
peekRequestBody rq = liftIO $ tryReadMVar (rqBody rq)

getBody :: RouteT Sitemap App L.ByteString
getBody = do
    req  <- askRq
    body <- liftIO $ peekRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return (L.pack "")

readAuthUserFromBodyAsList :: L.ByteString -> Maybe NewAccountData
readAuthUserFromBodyAsList bString = decode bString :: Maybe NewAccountData

getHttpMethod :: RouteT Sitemap App Method
getHttpMethod = do
    nullDir
    g <- rqMethod <$> askRq
    ok g
