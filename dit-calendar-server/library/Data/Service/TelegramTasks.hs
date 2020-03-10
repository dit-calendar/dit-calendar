{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.TelegramTasks (deleteTaskFromAllTelegramLinksImpl, addTelegramLinkToTaskImpl, removeTelegramLinkFromTaskImpl, TelegramTasksAssignmentService(..) ) where

import           Control.Monad.IO.Class
import           Data.Generics.Aliases    (orElse)
import           Data.List                (delete)
import           Data.Maybe               (fromJust)

import           AppContext               (App)
import           Data.Domain.Task         as Task
import           Data.Domain.TelegramLink as TelegramLink
import           Data.Domain.Types        (EitherResult)

import           Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo)
import           Data.Repository.TaskRepo (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo as TaskRepo
import qualified Data.Repository.TelegramLinkRepo as TelegramRepo



deleteTaskFromAllTelegramLinksImpl :: (MonadIO m, MonadDBTelegramRepo m) =>
                        Task -> m ()
deleteTaskFromAllTelegramLinksImpl task =
    foldr (\ x ->
      (>>) (do
        telegramLink <- TelegramRepo.findTelegramLinkById x
        TelegramRepo.updateTelegramLink (fromJust telegramLink) {assignedToTasks = delete (taskId task) (assignedToTasks (fromJust telegramLink))}
        )) 
    (return ()) $ Task.assignedTelegramLinks task

addTelegramLinkToTaskImpl :: (MonadDBTaskRepo m, MonadDBTelegramRepo m) =>
                Task -> TelegramLink -> m (EitherResult Task)
addTelegramLinkToTaskImpl task telegramLink =
    if taskId task `elem` assignedTelegramLinks task
    then return (Right task) -- do nothing and return same task
    else do
        TelegramRepo.updateTelegramLink telegramLink {assignedToTasks = taskId task : assignedToTasks telegramLink} 
        TaskRepo.updateTask task {assignedTelegramLinks = TelegramLink.chatId telegramLink : assignedTelegramLinks task}

removeTelegramLinkFromTaskImpl :: (MonadDBTaskRepo m, MonadDBTelegramRepo m) =>
                    Task -> TelegramLink -> m (EitherResult Task)
removeTelegramLinkFromTaskImpl task telegramLink = do
        TelegramRepo.updateTelegramLink telegramLink {assignedToTasks = delete (taskId task) (assignedToTasks telegramLink)} 
        TaskRepo.updateTask task {assignedTelegramLinks = delete (TelegramLink.chatId telegramLink) (assignedTelegramLinks task)}

class Monad m => TelegramTasksAssignmentService m where
    deleteTaskFromAllTelegramLinks :: Task -> m ()
    addTelegramLinkToTask :: Task -> TelegramLink -> m (EitherResult Task)
    removeTelegramLinkFromTask :: Task -> TelegramLink -> m (EitherResult Task)

instance (MonadDBTaskRepo App, MonadDBTelegramRepo App)
            => TelegramTasksAssignmentService App where
    deleteTaskFromAllTelegramLinks = deleteTaskFromAllTelegramLinksImpl
    addTelegramLinkToTask = addTelegramLinkToTaskImpl
    removeTelegramLinkFromTask = removeTelegramLinkFromTaskImpl
