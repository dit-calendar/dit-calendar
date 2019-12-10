{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.ResponseBuilder
    ( onUserExist
    , onEntryExist
    , onTaskExist
    , badRequest
    , okResponseJson
    , notImplemented
    , handleResponse
    , corsResponse
    , addCorsToResponse
    ) where

import           Data.Aeson                   (ToJSON, encode)
import           Data.ByteString.Lazy
import           Data.Text                    (Text)
import           Happstack.Server             (Method, Response, forbidden,
                                               notFound, ok, setHeader,
                                               toResponse, toResponseBS)

import qualified Data.ByteString.Char8        as T
import qualified Happstack.Server             as HServer (FilterMonad,
                                                          Happstack, badRequest,
                                                          forbidden, resp)

import           AppContext                   (App)
import           Data.Domain.CalendarEntry    (CalendarEntry)
import           Data.Domain.Task             (Task)
import           Data.Domain.Types            (EitherResult, EntryId,
                                               ResultError (..), TaskId, UserId)
import           Data.Domain.User             (User)
import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import           Data.Repository.UserRepo     (MonadDBUserRepo)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Repository.UserRepo     as UserRepo

preconditionFailed :: (HServer.FilterMonad Response m) => a -> m a
preconditionFailed = HServer.resp 412

onDBEntryExist :: (ToJSON dto, Monad m) => (Int -> m (Maybe entry)) -> Int -> (entry -> m (EitherResult dto)) -> m (EitherResult dto)
onDBEntryExist find i controllerFunction = do
    mDbEntry <- find i
    case mDbEntry of
        Nothing      -> return $ Left $ EntryNotFound i
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

onNothing :: HServer.FilterMonad Response m => String -> (a -> m Response) -> Maybe a -> m Response
onNothing message = maybe (okResponse message)

okResponse :: HServer.FilterMonad Response m => String -> m Response
okResponse message = ok $ toResponse message

corsResponse :: HServer.FilterMonad Response m => m Response
corsResponse =
    let res = toResponse ("" :: String)
     in ok $ addCorsHeaders res

addCorsHeaders :: Response -> Response
addCorsHeaders response =
    setHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE" $
    setHeader "Access-Control-Allow-Headers" "Content-Type" $
    setHeader "Access-Control-Allow-Origin" "http://localhost:8000" $
    setHeader "Access-Control-Allow-Credentials" "true" response

badRequest :: HServer.FilterMonad Response m  => String -> m Response
badRequest message = HServer.badRequest $ toResponse message

okResponseJson :: HServer.FilterMonad Response m  => ByteString -> m Response
okResponseJson object = ok $ toResponseBS (T.pack "application/json") object

preconditionFailedResponse :: HServer.FilterMonad Response m => Text -> m Response
preconditionFailedResponse message = preconditionFailed $ toResponse message

notImplemented :: HServer.FilterMonad Response m => Method -> m Response
notImplemented httpMethod = HServer.resp 405 $ toResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")

addCorsToResponse :: HServer.FilterMonad Response m => m Response -> m Response
addCorsToResponse resM = addCorsHeaders <$> resM
