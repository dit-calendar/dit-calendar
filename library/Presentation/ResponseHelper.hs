{-# LANGUAGE FlexibleContexts #-}

module Presentation.ResponseHelper
    ( onUserExist, onEntryExist, onTaskExist, okResponse,notImplemented  ) where

import           Happstack.Server                   (Method, ok, toResponse)

import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Task                   (Task)
import           Data.Domain.Types                  (EntryId, TaskId, UserId)
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO)
import           Data.Repository.Acid.Task          (TaskDAO)
import           Data.Repository.Acid.User          (UserDAO)

import           Presentation.AcidHelper            (CtrlV, CtrlV')

import qualified Data.Repository.Acid.CalendarEntry as CalendarDao
import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskDao
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.User          as UserDao
import qualified Data.Repository.Acid.User          as UserAcid


onUserExist :: UserDAO CtrlV' => UserId -> (User -> CtrlV) -> CtrlV
onUserExist i daoFunction = UserDao.query (UserAcid.UserById i) >>= (onNothing $ "Could not find a user with id " ++ show i) daoFunction

onEntryExist :: CalendarDAO CtrlV' => EntryId -> (CalendarEntry -> CtrlV) -> CtrlV
onEntryExist i daoFunction = CalendarDao.query (CalendarEntryAcid.EntryById i) >>= (onNothing $ "Could not find a entry with id " ++ show i) daoFunction

onTaskExist :: TaskDAO CtrlV' => TaskId -> (Task -> CtrlV) -> CtrlV
onTaskExist i daoFunction = do
    mTask <- TaskDao.query (TaskAcid.TaskById i)
    (onNothing $ "Could not find a task with id " ++ show i) daoFunction mTask

onNothing :: String -> (a -> CtrlV) -> Maybe a -> CtrlV
onNothing message = maybe (okResponse message)

okResponse :: String -> CtrlV
okResponse message = ok $ toResponse message

notImplemented :: Method -> CtrlV
notImplemented httpMethod = okResponse ("HTTP-Method: " ++ show httpMethod ++ " not implemented")
