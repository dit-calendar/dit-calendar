{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.TelegramTasks (deleteTaskFromAllTelegramLinksImpl, addTelegramLinkToTaskImpl,
    addNewTelegramLinkToTaskImpl, removeTelegramLinkFromTaskImpl, TelegramTasksAssignmentService(..) ) where

import           Control.Monad.IO.Class
import           Data.List                        (delete)
import           Data.Maybe                       (fromJust)

import           AppContext                       (App)
import           Data.Domain.Task                 as Task
import           Data.Domain.TelegramLink         as TelegramLink
import           Data.Domain.Types                (EitherResult)

import           Data.Repository.TaskRepo         (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo         as TaskRepo
import           Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo)
import qualified Data.Repository.TelegramLinkRepo as TelegramRepo


addNewTelegramLinkToTaskImpl :: (MonadDBTaskRepo m, MonadDBTelegramRepo m) =>
                TelegramLink -> Task ->  m (EitherResult Task)
addNewTelegramLinkToTaskImpl newEntity task = do
    newLink <- TelegramRepo.createTelegramLink newEntity
    addTelegramLinkToTaskImpl newLink task

deleteTaskFromAllTelegramLinksImpl :: (MonadIO m, MonadDBTelegramRepo m) =>
                        Task -> m ()
deleteTaskFromAllTelegramLinksImpl task =
    foldr (\ chatId ->
      (>>) (deleteTaskFromTelegramLink chatId task))
    (return ()) $ Task.assignedTelegramLinks task

deleteTaskFromTelegramLink chatId task = do
    telegramLink <- TelegramRepo.findTelegramLinkById chatId
    TelegramRepo.updateTelegramLink (fromJust telegramLink) {assignedToTasks = delete (taskId task) (assignedToTasks (fromJust telegramLink))}

addTelegramLinkToTaskImpl :: (MonadDBTaskRepo m, MonadDBTelegramRepo m) =>
                TelegramLink -> Task -> m (EitherResult Task)
addTelegramLinkToTaskImpl telegramLink task =
    if chatId telegramLink `elem` assignedTelegramLinks task
    then return (Right task) -- do nothing and return same task
    else do
        TelegramRepo.updateTelegramLink telegramLink {assignedToTasks = taskId task : assignedToTasks telegramLink}
        TaskRepo.updateTask task {assignedTelegramLinks = TelegramLink.chatId telegramLink : assignedTelegramLinks task}

removeTelegramLinkFromTaskImpl :: (MonadDBTaskRepo m, MonadDBTelegramRepo m) =>
                    Task -> TelegramLink -> m (EitherResult Task)
removeTelegramLinkFromTaskImpl task telegramLink =
    if chatId telegramLink `notElem` assignedTelegramLinks task
    then return (Right task) -- do nothing and return same task
    else do
        TelegramRepo.updateTelegramLink telegramLink {assignedToTasks = delete (taskId task) (assignedToTasks telegramLink)}
        TaskRepo.updateTask task {assignedTelegramLinks = delete (TelegramLink.chatId telegramLink) (assignedTelegramLinks task)}

class Monad m => TelegramTasksAssignmentService m where
    addNewTelegramLinkToTask :: TelegramLink -> Task  -> m (EitherResult Task)
    addTelegramLinkToTask :: TelegramLink -> Task -> m (EitherResult Task)
    deleteTaskFromAllTelegramLinks :: Task -> m ()
    removeTelegramLinkFromTask :: Task -> TelegramLink -> m (EitherResult Task)

instance (MonadDBTaskRepo App, MonadDBTelegramRepo App)
            => TelegramTasksAssignmentService App where
    addNewTelegramLinkToTask = addNewTelegramLinkToTaskImpl
    addTelegramLinkToTask = addTelegramLinkToTaskImpl
    deleteTaskFromAllTelegramLinks = deleteTaskFromAllTelegramLinksImpl
    removeTelegramLinkFromTask = removeTelegramLinkFromTaskImpl
