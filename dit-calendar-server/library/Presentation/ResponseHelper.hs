{-# LANGUAGE FlexibleContexts #-}

module Presentation.ResponseHelper
    ( onUserExist
    , onEntryExist
    , onTaskExist
    , okResponse
    , badRequest
    , okResponseJson
    , notImplemented
    , preconditionFailedResponse
    ) where

import           Data.Aeson                   (ToJSON, Value, encode)
import           Data.ByteString.Lazy
import           Data.Text                    as C (Text, pack)
import           Happstack.Server             (Method, Response, ok, toResponse,
                                               toResponseBS)

import qualified Data.ByteString.Char8        as T
import qualified Happstack.Server             as HServer (FilterMonad,
                                                          badRequest, resp)

import           AcidHelper                   (App)
import           Data.Domain.CalendarEntry    (CalendarEntry)
import           Data.Domain.Task             (Task)
import           Data.Domain.Types            (EitherResponse, EntryId, TaskId,
                                               UserId)
import           Data.Domain.User             (User)

import qualified Data.Repository.CalendarRepo as CalendarRepo
import qualified Data.Repository.TaskRepo     as TaskRepo
import qualified Data.Repository.UserRepo     as UserRepo

preconditionFailed :: (HServer.FilterMonad Response m) => a -> m a
preconditionFailed = HServer.resp 412

onDBEntryExist :: ToJSON dto => (Int -> App (Maybe entry)) -> Int ->  (entry -> App (EitherResponse dto)) -> App Response
onDBEntryExist find i daoFunction = do
    mUser <- find i
    case mUser of
        Nothing -> okResponse $ "Could not find a db entry with id " ++ show i
        Just user -> do
            resp <- daoFunction user
            case resp of
                Left errorMessage -> preconditionFailedResponse errorMessage
                Right dto         -> okResponseJson $ encode dto

onUserExist :: ToJSON dto => UserId -> (User -> App (EitherResponse dto)) -> App Response
onUserExist = onDBEntryExist UserRepo.findUserById

onEntryExist :: ToJSON dto => EntryId -> (CalendarEntry -> App (EitherResponse dto)) -> App Response
onEntryExist = onDBEntryExist CalendarRepo.findCalendarById

onTaskExist :: ToJSON dto => TaskId -> (Task -> App (EitherResponse dto)) -> App Response
onTaskExist = onDBEntryExist TaskRepo.findTaskById

onNothing :: String -> (a -> App Response) -> Maybe a -> App Response
onNothing message = maybe (okResponse message)

okResponse :: String -> App Response
okResponse message = ok $ toResponse message

badRequest :: String -> App Response
badRequest message = HServer.badRequest $ toResponse message

okResponseJson :: ByteString -> App Response
okResponseJson object = ok $ toResponseBS (T.pack "application/json") object

preconditionFailedResponse :: Text -> App Response
preconditionFailedResponse message = preconditionFailed $ toResponse message

notImplemented :: Method -> App Response
notImplemented httpMethod = okResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")
