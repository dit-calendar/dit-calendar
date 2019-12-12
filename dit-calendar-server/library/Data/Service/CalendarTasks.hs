{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarTasks (CalendarTasksService(..), getCalendarTasksIml, deleteCalendarsTasksImpl) where

import           Data.Maybe                (fromJust)

import           AppContext                (App)
import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (TaskId)
import           Data.Repository.TaskRepo  (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo  as MonadDBTaskRepo

deleteCalendarsTasksImpl :: MonadDBTaskRepo m => CalendarEntry -> m ()
deleteCalendarsTasksImpl calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.findTaskById x
        MonadDBTaskRepo.deleteTask (fromJust task) ))
    (return ()) $ CalendarEntry.tasks calendar

getCalendarTasksIml :: MonadDBTaskRepo m => CalendarEntry -> m [Task]
getCalendarTasksIml calendar = mapM getTaskWithFail (CalendarEntry.tasks calendar)

-- https://en.wikibooks.org/wiki/Haskell/do_notation#The_fail_method
getTaskWithFail :: (MonadDBTaskRepo m) => TaskId -> m Task
getTaskWithFail taskId = do
    Just task <- MonadDBTaskRepo.findTaskById taskId
    return task

class Monad m => CalendarTasksService m where
    getCalendarTasks :: CalendarEntry -> m [Task]
    deleteCalendarsTasks :: CalendarEntry -> m ()

instance MonadDBTaskRepo App => CalendarTasksService App where
    getCalendarTasks = getCalendarTasksIml
    deleteCalendarsTasks = deleteCalendarsTasksImpl
