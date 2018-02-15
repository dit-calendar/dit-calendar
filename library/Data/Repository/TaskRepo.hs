{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.TaskRepo
    ( updateDescription, deleteTask, createTask, updateTask, getTask ) where

import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import qualified Happstack.Foundation          as Foundation

import Data.Repository.Acid.Task      ( MonadDBTask(..) )
import Data.Domain.CalendarEntry               as CalendarEntry
import Data.Domain.Task                        as Task
import Controller.AcidHelper          ( CtrlV' )
import Data.Domain.Types              ( TaskId )

import qualified Data.Repository.Acid.Task    as TaskAcid

instance MonadDBTask CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTask :: MonadDBTask m => Task -> m ()
updateTask task = update $ TaskAcid.UpdateTask task

updateDescription :: MonadDBTask m => Task -> String -> m ()
updateDescription task newDescription = updateTask task {Task.description = newDescription}

deleteTask :: MonadDBTask m => Task -> m ()
deleteTask task = delete $ TaskAcid.DeleteTask $ taskId task

createTask :: MonadDBTask m => String -> m Task
createTask description = 
    let task = Task { Task.description = description
                    , taskId  = undefined
                    , belongingUsers = []
                    } in
        create $ TaskAcid.NewTask task

getTask :: (MonadDBTask m, MonadIO m) => TaskId -> m Task
getTask taskId =
    fromJust <$> query (TaskAcid.TaskById taskId)
