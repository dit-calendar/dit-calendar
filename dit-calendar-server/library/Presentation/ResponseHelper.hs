{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentation.ResponseHelper
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
import           Happstack.Server             (Method, Response, notFound, ok,
                                               setHeader, toResponse,
                                               toResponseBS)

import qualified Data.ByteString.Char8        as T
import qualified Happstack.Server             as HServer (FilterMonad,
                                                          Happstack, badRequest,
                                                          resp)

import           AcidHelper                   (App)
import           Data.Domain.CalendarEntry    (CalendarEntry)
import           Data.Domain.Task             (Task)
import           Data.Domain.Types            (EitherResult, EntryId,
                                               ResultError (..), TaskId,
                                               UserId)
import           Data.Domain.User             (User)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Repository.UserRepo     as UserRepo

preconditionFailed :: (HServer.FilterMonad Response m) => a -> m a
preconditionFailed = HServer.resp 412

onDBEntryExist :: ToJSON dto => (Int -> App (Maybe entry)) -> Int -> (entry -> App (EitherResult dto)) -> App (EitherResult dto)
onDBEntryExist find i controllerFunction = do
    mDbEntry <- find i
    case mDbEntry of
        Nothing -> return $ Left $ EntryNotFound i
        Just dbEntry -> controllerFunction dbEntry

handleResponse :: ToJSON dto => EitherResult dto -> App Response
handleResponse (Left OptimisticLocking) = preconditionFailedResponse "\"version is not set or not equal with database\""
handleResponse (Left (EntryNotFound i)) = notFound $ toResponse $ "\"Could not find a db entry with id " ++ show i ++ "\""
handleResponse (Right dto)     = okResponseJson $ encode dto

onUserExist :: ToJSON dto => UserId -> (User -> App (EitherResult dto)) -> App (EitherResult dto)
onUserExist = onDBEntryExist UserRepo.findUserById

onEntryExist :: ToJSON dto => EntryId -> (CalendarEntry -> App (EitherResult dto)) -> App (EitherResult dto)
onEntryExist = onDBEntryExist CalendarRepo.findCalendarById

onTaskExist :: ToJSON dto => TaskId -> (Task -> App (EitherResult dto)) -> App (EitherResult dto)
onTaskExist = onDBEntryExist TaskRepo.findTaskById

onNothing :: String -> (a -> App Response) -> Maybe a -> App Response
onNothing message = maybe (okResponse message)

okResponse :: String -> App Response
okResponse message = ok $ toResponse message

corsResponse :: App Response
corsResponse =
    let res = toResponse ("" :: String)
     in ok $ addCorsHeaders res

addCorsHeaders :: Response -> Response
addCorsHeaders response =
    setHeader "Access-Control-Allow-Methods" "POST, GET, PUT, DELETE" $
    setHeader "Access-Control-Allow-Headers" "Content-Type" $
    setHeader "Access-Control-Allow-Origin" "http://localhost:8000" $
    setHeader "Access-Control-Allow-Credentials" "true" response

badRequest :: String -> App Response
badRequest message = HServer.badRequest $ toResponse message

okResponseJson :: ByteString -> App Response
okResponseJson object = ok $ toResponseBS (T.pack "application/json") object

preconditionFailedResponse :: Text -> App Response
preconditionFailedResponse message = preconditionFailed $ toResponse message

notImplemented :: Method -> App Response
notImplemented httpMethod = HServer.resp 405 $ toResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")

addCorsToResponse :: HServer.Happstack m => m Response -> m Response
addCorsToResponse resM = addCorsHeaders <$> resM
