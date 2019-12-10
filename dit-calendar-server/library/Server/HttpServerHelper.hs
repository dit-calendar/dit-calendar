module Server.HttpServerHelper
    ( getBody
    , readAuthUserFromBodyAsList
    , getHttpMethod
    ) where

import           Control.Concurrent.MVar              (tryReadMVar)
import           Control.Monad.IO.Class               (MonadIO, liftIO)

import           Data.Aeson                           (decode)
import           Happstack.Authenticate.Password.Core (NewAccountData)
import           Happstack.Server                     (Method,
                                                       Request (rqMethod),
                                                       Response, ServerPartT,
                                                       askRq, mapServerPartT,
                                                       nullDir, ok, rqBody,
                                                       unBody)
import           Happstack.Server.Types               (Request, RqBody)

import           AppContext                           (App)
import           Presentation.Route.PageEnum          (Sitemap)
import           Web.Routes                           (RouteT)

import qualified Data.ByteString.Lazy.Char8           as L

peekRequestBody :: (MonadIO m) => Request -> m (Maybe RqBody)
peekRequestBody rq = liftIO $ tryReadMVar (rqBody rq)

getBody :: App L.ByteString
getBody = do
    req <- askRq
    body <- liftIO $ peekRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return (L.pack "")

readAuthUserFromBodyAsList :: L.ByteString -> Maybe NewAccountData
readAuthUserFromBodyAsList bString = decode bString :: Maybe NewAccountData

getHttpMethod :: App Method
getHttpMethod = do
    nullDir
    g <- rqMethod <$> askRq
    ok g
