{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TelegramLinkRepo where

import           Control.Monad.IO.Class            (MonadIO)

import           AppContext                        (App)
import           Data.Domain.Task
import           Data.Domain.TelegramLink
import           Data.Domain.Types                 (EitherResult,
                                                    TelegramChatId)
import           Data.Repository.Acid.TelegramLink (TelegramDAO (..),
                                                    TelegramLinkById (..),
                                                    UpdateTelegramLink (..))
import           Server.AcidInitializer

import qualified Data.List                         as List
import qualified Server.HappstackHelper            as Foundation

instance TelegramDAO App where
    create = Foundation.update
    update = Foundation.update
    query  = Foundation.query

findTelegramLinkByIdImpl :: (TelegramDAO m, MonadIO m) => TelegramChatId -> m (Maybe TelegramLink)
findTelegramLinkByIdImpl = query . TelegramLinkById

addTaskToTelegramLinkImpl :: TelegramDAO m => TelegramLink -> Task -> m (EitherResult TelegramLink)
addTaskToTelegramLinkImpl user taskId =
    undefined

deleteTaskFromTelegramLinkImpl :: TelegramDAO m => TelegramLink -> Task -> m (EitherResult TelegramLink)
deleteTaskFromTelegramLinkImpl user task =
    undefined

updateTelegramLink :: TelegramDAO m => TelegramLink -> m (EitherResult TelegramLink)
updateTelegramLink = update . UpdateTelegramLink

class Monad m => MonadDBTelegramRepo m where
    findTelegramLinkById :: TelegramChatId -> m (Maybe TelegramLink)
    addTaskToTelegramLink :: TelegramLink -> Task -> m (EitherResult TelegramLink)
    deleteTaskFromTelegramLink :: TelegramLink -> Task -> m (EitherResult TelegramLink)

instance TelegramDAO App => MonadDBTelegramRepo App where
    findTelegramLinkById = findTelegramLinkByIdImpl
    addTaskToTelegramLink = addTaskToTelegramLinkImpl
    deleteTaskFromTelegramLink = deleteTaskFromTelegramLinkImpl