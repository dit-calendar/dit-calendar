{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo(..)) where

import           Control.Monad.IO.Class            (MonadIO)

import           AppContext                        (App, AppContext,
                                                    getCurrentUser)
import           Data.Domain.Task
import           Data.Domain.TelegramLink
import           Data.Domain.Types                 (EitherResult,
                                                    TelegramChatId, UserId)
import           Data.Domain.User                  (userId)
import           Data.Repository.Acid.TelegramLink (NewTelegramLink (..),
                                                    TelegramDAO (..),
                                                    TelegramLinkById (..),
                                                    UpdateTelegramLink (..))
import           Data.Repository.PermissionControl (executeUnderUserPermission)
import           Server.AcidInitializer

import qualified Data.List                         as List
import qualified Server.HappstackHelper            as Foundation

instance TelegramDAO App where
    create = Foundation.update
    update = Foundation.update
    query  = Foundation.query

createTelegramLinkImpl :: TelegramDAO m => TelegramLink -> m TelegramLink
createTelegramLinkImpl = create . NewTelegramLink

findTelegramLinkByIdImpl :: (TelegramDAO m, AppContext m, MonadIO m) => TelegramChatId -> m (Maybe TelegramLink)
findTelegramLinkByIdImpl chatId = do
    mUser <- getCurrentUser
    case mUser of
        Just user -> query $ TelegramLinkById (chatId, userId user)
        Nothing   -> return Nothing

updateTelegramLinkImpl :: (TelegramDAO m, AppContext m) => TelegramLink -> m (EitherResult TelegramLink)
updateTelegramLinkImpl telegramLink = executeUnderUserPermission telegramLink (update $ UpdateTelegramLink telegramLink)

class Monad m => MonadDBTelegramRepo m where
    findTelegramLinkById :: TelegramChatId -> m (Maybe TelegramLink)
    updateTelegramLink :: TelegramLink -> m (EitherResult TelegramLink)
    createTelegramLink :: TelegramLink -> m TelegramLink

instance TelegramDAO App => MonadDBTelegramRepo App where
    findTelegramLinkById = findTelegramLinkByIdImpl
    updateTelegramLink = updateTelegramLinkImpl
    createTelegramLink = createTelegramLinkImpl
