{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( deleteTaskImpl, createTaskImpl, updateTaskImpl, findTaskByIdImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default                      (def)

import           AppContext                        (App, AppContext)
import           Data.Domain.CalendarEntry         as CalendarEntry
import           Data.Domain.Task                  as Task
import           Data.Domain.Types                 (Description, EitherResult,
                                                    TaskId)
import           Data.Repository.Acid.Task         (TaskDAO (..))
import           Data.Repository.PermissionControl (executeUnderUserPermission)
import           Server.AcidInitializer

import qualified Data.Repository.Acid.Task         as TaskAcid
import qualified Server.HappstackHelper            as Foundation

instance TaskDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: (TaskDAO m, AppContext m ) => Task -> m (EitherResult Task)
updateTaskImpl task = executeUnderUserPermission
    task (update $ TaskAcid.UpdateTask task)

--TODO permission for delete
deleteTaskImpl :: TaskDAO m => Task -> m ()
deleteTaskImpl = delete . TaskAcid.DeleteTask . taskId

createTaskImpl :: TaskDAO m => Task -> m Task
createTaskImpl = create . TaskAcid.NewTask

findTaskByIdImpl :: (TaskDAO m, MonadIO m) => TaskId -> m (Maybe Task)
findTaskByIdImpl = query . TaskAcid.TaskById


class Monad m => MonadDBTaskRepo m where
    createTask        :: Task -> m Task
    findTaskById      :: TaskId -> m (Maybe Task)
    updateTask        :: Task   -> m (EitherResult Task)
    deleteTask        :: Task   -> m ()

instance TaskDAO App => MonadDBTaskRepo App where
    createTask        = createTaskImpl
    findTaskById      = findTaskByIdImpl
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl
