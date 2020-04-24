{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.TelegramLink (findTelegramLinksByIdsImpl, TelegramLinkService(..)) where

import           Data.Maybe                       (catMaybes)

import           AppContext                       (App)
import           Data.Domain.TelegramLink         as TelegramLink
import           Data.Domain.Types                (TelegramChatId)

import           Data.Repository.TelegramLinkRepo (MonadDBTelegramRepo)
import qualified Data.Repository.TelegramLinkRepo as TelegramRepo

findTelegramLinksByIdsImpl :: MonadDBTelegramRepo m => [TelegramChatId] -> m [TelegramLink]
findTelegramLinksByIdsImpl =  fmap catMaybes . mapM TelegramRepo.findTelegramLinkById

class Monad m => TelegramLinkService m where
    findTelegramLinksByIds :: [TelegramChatId] -> m [TelegramLink]

instance MonadDBTelegramRepo App
            => TelegramLinkService App where
    findTelegramLinksByIds = findTelegramLinksByIdsImpl
