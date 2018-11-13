{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( updateDescription, deleteTaskImpl, createTaskImpl, updateTaskImpl, getTaskImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Maybe                (fromJust)

import qualified Happstack.Foundation      as Foundation

import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (TaskId, Description)
import           Data.Repository.Acid.Task (TaskDAO (..))
import           Presentation.AcidHelper   (App)

import qualified Data.Repository.Acid.Task as TaskAcid

instance TaskDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: TaskDAO m => Task -> m ()
updateTaskImpl task = update $ TaskAcid.UpdateTask task

updateDescription :: TaskDAO m => Task -> Description -> m ()
updateDescription task newDescription = updateTaskImpl task {Task.description = newDescription}

deleteTaskImpl :: TaskDAO m => Task -> m ()
deleteTaskImpl task = delete $ TaskAcid.DeleteTask $ taskId task

createTaskImpl :: TaskDAO m => Description -> m Task
createTaskImpl description =
    let task = Task { Task.description = description
                    , taskId  = 0
                    , belongingUsers = []
                    } in
        create $ TaskAcid.NewTask task

getTaskImpl :: (TaskDAO m, MonadIO m) => TaskId -> m Task
getTaskImpl taskId =
    fromJust <$> query (TaskAcid.TaskById taskId)



class (Monad m, TaskDAO App) => MonadDBTaskRepo m where
    updateTask        :: Task   -> m ()
    deleteTask        :: Task   -> m ()
    createTask        :: Description -> m Task
    getTask           :: TaskId -> m Task

instance MonadDBTaskRepo App where
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl
    createTask        = createTaskImpl
    getTask           = getTaskImpl
