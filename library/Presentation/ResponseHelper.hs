{-# LANGUAGE FlexibleContexts #-}

module Presentation.ResponseHelper
    ( onUserExist, onEntryExist, onTaskExist, okResponse, okResponseJson,notImplemented  ) where

import           Happstack.Server                   (Method, Response, ok,
                                                     toResponse)
import           Data.Aeson                         (Value)
import           Data.ByteString.Lazy

import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Task                   (Task)
import           Data.Domain.Types                  (EntryId, TaskId, UserId)
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO)
import           Data.Repository.Acid.Task          (TaskDAO)
import           Data.Repository.Acid.User          (UserDAO)

import           Presentation.AcidHelper            (App)

import qualified Data.Repository.Acid.CalendarEntry as CalendarDao
import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskDao
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.User          as UserDao
import qualified Data.Repository.Acid.User          as UserAcid


onUserExist :: UserDAO App => UserId -> (User -> App Response) -> App Response
onUserExist i daoFunction = UserDao.query (UserAcid.UserById i) >>= (onNothing $ "Could not find a user with id " ++ show i) daoFunction

onEntryExist :: CalendarDAO App => EntryId -> (CalendarEntry -> App Response) -> App Response
onEntryExist i daoFunction = CalendarDao.query (CalendarEntryAcid.EntryById i) >>= (onNothing $ "Could not find a entry with id " ++ show i) daoFunction

onTaskExist :: TaskDAO App => TaskId -> (Task -> App Response) -> App Response
onTaskExist i daoFunction = do
    mTask <- TaskDao.query (TaskAcid.TaskById i)
    (onNothing $ "Could not find a task with id " ++ show i) daoFunction mTask

onNothing :: String -> (a -> App Response) -> Maybe a -> App Response
onNothing message = maybe (okResponse message)

okResponse :: String -> App Response
okResponse message = ok $ toResponse message

okResponseJson :: ByteString -> App Response
okResponseJson object = ok $ toResponse object

notImplemented :: Method -> App Response
notImplemented httpMethod = okResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")
