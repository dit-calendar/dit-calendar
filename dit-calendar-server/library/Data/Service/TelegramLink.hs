{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.TelegramLink (findTelegramLinksByIdsImpl, createTelegramLinkImpl, TelegramLinkService(..)) where

import           Data.Maybe                       (catMaybes)

import           AppContext                       (App)
import           Data.Domain.TelegramLink         as TelegramLink
import           Data.Domain.Types                (TelegramChatId, UserId)

import           Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo)
import qualified Data.Repository.TelegramLinkRepo as TelegramRepo

findTelegramLinksByIdsImpl :: MonadDBTelegramRepo m => [TelegramChatId] -> m [TelegramLink]
findTelegramLinksByIdsImpl =  fmap catMaybes . mapM TelegramRepo.findTelegramLinkById

createTelegramLinkImpl :: MonadDBTelegramRepo m => UserId -> TelegramLink -> m TelegramLink
createTelegramLinkImpl userId newEntity = TelegramRepo.createTelegramLink newEntity {TelegramLink.owner = userId}

class Monad m => TelegramLinkService m where
    findTelegramLinksByIds :: [TelegramChatId] -> m [TelegramLink]
    createTelegramLink :: UserId -> TelegramLink -> m TelegramLink

instance MonadDBTelegramRepo App
            => TelegramLinkService App where
    findTelegramLinksByIds = findTelegramLinksByIdsImpl
    createTelegramLink = createTelegramLinkImpl
