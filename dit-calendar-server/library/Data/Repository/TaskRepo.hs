{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( deleteTaskImpl, createTaskImpl, updateTaskImpl, findTaskByIdImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default              (def)

import           AppContext                (App)
import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (Description, EitherResult, TaskId)
import           Data.Repository.Acid.Task (TaskDAO (..))
import           Server.AcidInitializer

import qualified Data.Repository.Acid.Task as TaskAcid
import qualified Server.HappstackHelper    as Foundation

instance TaskDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: TaskDAO m => Task -> m (EitherResult Task)
updateTaskImpl = update . TaskAcid.UpdateTask

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
