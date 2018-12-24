{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( deleteTaskImpl, createTaskImpl, updateTaskImpl, findTaskByIdImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default              (def)
import           Data.Maybe                (fromJust)

import qualified Happstack.Foundation      as Foundation

import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (Description, TaskId)
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

deleteTaskImpl :: TaskDAO m => Task -> m ()
deleteTaskImpl task = delete $ TaskAcid.DeleteTask $ taskId task

createTaskImpl :: TaskDAO m => Description -> m Task
createTaskImpl description =
    let task = def { Task.description = description
                    , startTime=Nothing
                    , endTime=Nothing
                    } in
        create $ TaskAcid.NewTask task

findTaskByIdImpl :: (TaskDAO m, MonadIO m) => TaskId -> m Task
findTaskByIdImpl taskId =
    fromJust <$> query (TaskAcid.TaskById taskId)


class (Monad m, TaskDAO App) => MonadDBTaskRepo m where
    createTask        :: Description -> m Task
    findTaskById      :: TaskId -> m Task
    updateTask        :: Task   -> m ()
    deleteTask        :: Task   -> m ()

instance MonadDBTaskRepo App where
    createTask        = createTaskImpl
    findTaskById      = findTaskByIdImpl
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl
