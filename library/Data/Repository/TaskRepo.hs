{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( deleteTaskImpl, createTaskImpl, updateTaskImpl, findTaskByIdImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default               (def)
import           Data.Maybe                 (fromJust)

import qualified Happstack.Foundation       as Foundation

import           Data.Domain.CalendarEntry  as CalendarEntry
import           Data.Domain.Task           as Task
import           Data.Domain.Types          (Description, TaskId)
import           Data.Repository.Acid.Task  (TaskDAO (..))
import           Data.Repository.Acid.Types (UpdateReturn)
import           AcidHelper    (App)

import qualified Data.Repository.Acid.Task  as TaskAcid

instance TaskDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: TaskDAO m => Task -> m (UpdateReturn Task)
updateTaskImpl task = update $ TaskAcid.UpdateTask task

deleteTaskImpl :: TaskDAO m => TaskId -> m ()
deleteTaskImpl taskId = delete $ TaskAcid.DeleteTask taskId

createTaskImpl :: TaskDAO m => Task -> m Task
createTaskImpl task = create $ TaskAcid.NewTask task

findTaskByIdImpl :: (TaskDAO m, MonadIO m) => TaskId -> m Task
findTaskByIdImpl taskId =
    fromJust <$> query (TaskAcid.TaskById taskId)


class (Monad m, TaskDAO App) => MonadDBTaskRepo m where
    createTask        :: Task -> m Task
    findTaskById      :: TaskId -> m Task
    updateTask        :: Task   -> m (UpdateReturn Task)
    deleteTask        :: TaskId   -> m ()

instance MonadDBTaskRepo App where
    createTask        = createTaskImpl
    findTaskById      = findTaskByIdImpl
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl
