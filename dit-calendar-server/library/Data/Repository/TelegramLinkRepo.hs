{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TelegramLinkRepo where

import           AppContext                        (App)
import           Data.Domain.Task
import           Data.Domain.TelegramLink
import           Data.Domain.Types                 (EitherResult)
import           Data.Repository.Acid.TelegramLink (TelegramDAO (..), UpdateTelegramLink(..))

import qualified Server.HappstackHelper            as Foundation
import qualified Data.List                 as List

instance TelegramDAO App where
    create = Foundation.update
    update = Foundation.update

addTaskToTelegramLinkImpl :: TelegramDAO m => TelegramLink -> Task -> m (EitherResult TelegramLink)
addTaskToTelegramLinkImpl user taskId =
    undefined

deleteTaskFromTelegramLinkImpl :: TelegramDAO m => TelegramLink -> Task -> m (EitherResult TelegramLink)
deleteTaskFromTelegramLinkImpl user task =
    undefined

updateTelegramLink :: TelegramDAO m => TelegramLink -> m (EitherResult TelegramLink)
updateTelegramLink = update . UpdateTelegramLink

class Monad m => MonadDBTelegramRepo m where
    addTaskToTelegramLink :: TelegramLink -> Task -> m (EitherResult TelegramLink)
    deleteTaskFromTelegramLink :: TelegramLink -> Task -> m (EitherResult TelegramLink)

instance TelegramDAO App => MonadDBTelegramRepo App where
    addTaskToTelegramLink = addTaskToTelegramLinkImpl
    deleteTaskFromTelegramLink = deleteTaskFromTelegramLinkImpl
