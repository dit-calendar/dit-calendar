{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.ResponseBuilder
    ( onUserExist
    , onEntryExist
    , onTaskExist
    , onTelegramLinkExist
    , badRequest
    , okResponseJson
    , notImplemented
    , handleResponse
    , corsResponse
    , addCorsToResponse
    ) where

import           Data.Aeson                       (ToJSON, encode)
import           Data.ByteString.Lazy             (ByteString)
import           Data.Char                        (toLower)
import           Data.Text                        (Text)
import           Happstack.Server                 (Method, Response, forbidden,
                                                   notFound, ok, setHeader,
                                                   toResponse, toResponseBS)

import qualified Data.ByteString.Char8            as T
import qualified Happstack.Server                 as HServer (FilterMonad,
                                                              Happstack,
                                                              badRequest,
                                                              forbidden, resp)

import           AppContext                       (App)
import           Conf.Config                      (CorsConfig (..))
import           Data.Domain.CalendarEntry        (CalendarEntry)
import           Data.Domain.Task                 (Task)
import           Data.Domain.TelegramLink         (TelegramLink)
import           Data.Domain.Types                (EitherResult, Entity,
                                                   EntryId, ResultError (..),
                                                   TaskId, TelegramChatId,
                                                   UserId)
import           Data.Domain.User                 (User)
import           Data.Repository.CalendarRepo     (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo         (MonadDBTaskRepo)
import           Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo)
import           Data.Repository.UserRepo         (MonadDBUserRepo)

import qualified Data.Repository.CalendarRepo     as CalendarRepo
import qualified Data.Repository.TaskRepo         as TaskRepo
import qualified Data.Repository.TelegramLinkRepo as TelegramRepo
import qualified Data.Repository.UserRepo         as UserRepo

preconditionFailed :: (HServer.FilterMonad Response m) => a -> m a
preconditionFailed = HServer.resp 412

onDBEntryExist :: (ToJSON dto, Monad m, Show key) => (key -> m (Maybe entry)) -> key -> (entry -> m (EitherResult dto)) -> m (EitherResult dto)
onDBEntryExist find i controllerFunction = do
    mDbEntry <- find i
    case mDbEntry of
        Nothing      -> return $ Left $ EntryNotFound (show i)
        Just dbEntry -> controllerFunction dbEntry

handleResponse :: (ToJSON dto, HServer.FilterMonad Response m ) => EitherResult dto -> m Response
handleResponse eitherResult = case eitherResult of
    Left error -> case error of
        OptimisticLocking -> preconditionFailedResponse "\"version is not set or not equal with database\""
        EntryNotFound i -> notFound $ toResponse $ "\"Could not find a db entry with id " ++ show i ++ "\""
        PermissionAccessInsufficient -> forbidden $ toResponse ("\"Sorry, it is forbidden.\""::ByteString)
    Right dto -> okResponseJson $ encode dto

onUserExist :: (ToJSON dto, MonadDBUserRepo m) => UserId -> (User -> m (EitherResult dto)) -> m (EitherResult dto)
onUserExist = onDBEntryExist UserRepo.findUserById

onEntryExist :: (ToJSON dto, MonadDBCalendarRepo m) => EntryId -> (CalendarEntry -> m (EitherResult dto)) -> m (EitherResult dto)
onEntryExist = onDBEntryExist CalendarRepo.findCalendarById

onTaskExist :: (ToJSON dto, MonadDBTaskRepo m)  => TaskId -> (Task -> m (EitherResult dto)) -> m (EitherResult dto)
onTaskExist = onDBEntryExist TaskRepo.findTaskById

onTelegramLinkExist :: (ToJSON dto, MonadDBTelegramRepo m)
    => TelegramChatId -> (TelegramLink -> m (EitherResult dto)) -> m (EitherResult dto)
onTelegramLinkExist = onDBEntryExist TelegramRepo.findTelegramLinkById

onNothing :: HServer.FilterMonad Response m => String -> (a -> m Response) -> Maybe a -> m Response
onNothing message = maybe (okResponse message)

okResponse :: HServer.FilterMonad Response m => String -> m Response
okResponse message = ok $ toResponse message

corsResponse :: HServer.FilterMonad Response m => CorsConfig -> m Response
corsResponse corsConfig =
    let res = toResponse ("" :: String)
     in ok $ addCorsHeaders corsConfig res

addCorsHeaders :: CorsConfig ->  Response -> Response
addCorsHeaders corsConfig response =
    setHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE, OPTIONS" $
    setHeader "Access-Control-Allow-Headers" "Content-Type" $
    setHeader "Access-Control-Allow-Origin" (corsConfigAllowOrigin corsConfig) $
    setHeader "Access-Control-Allow-Credentials" (map toLower (show $ corsConfigAllowCredentials corsConfig)) response

badRequest :: HServer.FilterMonad Response m  => String -> m Response
badRequest message = HServer.badRequest $ toResponse message

okResponseJson :: HServer.FilterMonad Response m  => ByteString -> m Response
okResponseJson object = ok $ toResponseBS (T.pack "application/json") object

preconditionFailedResponse :: HServer.FilterMonad Response m => Text -> m Response
preconditionFailedResponse message = preconditionFailed $ toResponse message

notImplemented :: HServer.FilterMonad Response m => Method -> m Response
notImplemented httpMethod = HServer.resp 405 $ toResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")

addCorsToResponse :: HServer.FilterMonad Response m => CorsConfig -> m Response -> m Response
addCorsToResponse corsConfig resM = addCorsHeaders corsConfig <$> resM
