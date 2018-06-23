{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( updateDescription, deleteTaskImpl, createTaskImpl, updateTaskImpl, getTaskImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                         (fromJust)

import qualified Happstack.Foundation               as Foundation

import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.Task                   as Task
import           Data.Domain.Types                  (TaskId)
import           Presentation.AcidHelper            (CtrlV')

import           Data.Repository.Acid.CalendarEntry (MonadDBCalendar)
import           Data.Repository.Acid.Task          (MonadDBTask (..))
import           Data.Repository.Acid.User          (MonadDBUser)

import qualified Data.Repository.Acid.Task          as TaskAcid

instance MonadDBTask CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: MonadDBTask m => Task -> m ()
updateTaskImpl task = update $ TaskAcid.UpdateTask task

updateDescription :: MonadDBTask m => Task -> String -> m ()
updateDescription task newDescription = updateTaskImpl task {Task.description = newDescription}

deleteTaskImpl :: MonadDBTask m => Task -> m ()
deleteTaskImpl task = delete $ TaskAcid.DeleteTask $ taskId task

createTaskImpl :: MonadDBTask m => String -> m Task
createTaskImpl description =
    let task = Task { Task.description = description
                    , taskId  = undefined
                    , belongingUsers = []
                    } in
        create $ TaskAcid.NewTask task

getTaskImpl :: (MonadDBTask m, MonadIO m) => TaskId -> m Task
getTaskImpl taskId =
    fromJust <$> query (TaskAcid.TaskById taskId)



class Monad m => MonadDBTaskRepo m where
    updateTask        :: Task   -> m ()
    deleteTask        :: Task   -> m ()
    createTask        :: String -> m Task
    getTask           :: TaskId -> m Task

instance (MonadDBUser CtrlV', MonadDBTask CtrlV', MonadDBCalendar CtrlV')
        => MonadDBTaskRepo CtrlV' where
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl
    createTask        = createTaskImpl
    getTask           = getTaskImpl
