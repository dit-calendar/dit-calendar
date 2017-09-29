{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.TaskRepo
    ( updateDescription, deleteTask, createTask, updateTask, getTask ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.CalendarEntry               as CalendarEntry
import Data.Domain.Task                        as Task
import Data.Domain.Types            ( TaskId )

import qualified Data.Repository.MonadDB.Task     as DBRepo
import qualified Data.Repository.Acid.TaskAcid    as TaskAcid


updateTask :: DBRepo.MonadDBTask m => Task -> m ()
updateTask task = DBRepo.update $ TaskAcid.UpdateTask task

updateDescription :: DBRepo.MonadDBTask m => Task -> String -> m ()
updateDescription task newDescription = updateTask task {Task.description = newDescription}

deleteTask :: DBRepo.MonadDBTask m => Task -> m ()
deleteTask task = DBRepo.delete $ TaskAcid.DeleteTask $ taskId task

createTask :: DBRepo.MonadDBTask m => String -> m Task
createTask description = let task = Task { Task.description = description
                        , taskId  = undefined
                        , belongingUsers = []
                        } in
        DBRepo.create $ TaskAcid.NewTask task

getTask :: (DBRepo.MonadDBTask m, MonadIO m) => TaskId -> m Task
getTask taskId = do
        mTask <- DBRepo.query (TaskAcid.TaskById taskId)
        return $ fromJust mTask
