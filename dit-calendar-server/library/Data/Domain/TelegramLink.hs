{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.TelegramLink
    ( TelegramLink(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Text         (Text)

import           Data.Domain.Types (Entity (..), TaskId, TelegramChatId, UserId)

data TelegramLink = TelegramLink
    { chatId          :: TelegramChatId
    , telegramUserId  :: Int
    , userName        :: Maybe Text
    , firstName       :: Maybe Text
    , assignedToTasks :: [TaskId]
    , owner           :: UserId
    , version         :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''TelegramLink)

instance Entity TelegramLink (TelegramChatId, UserId) where
    setId telegramLink (newChatId, newUserId) = telegramLink {chatId = newChatId, owner = newUserId}
    getId a = (chatId a, owner a)
    getUsersAccessRestriction a = [owner a]
    getVersion = version
    setVersion telegramLink newVersion = telegramLink {version = newVersion}

instance Default TelegramLink where
    def = TelegramLink {chatId = -1, assignedToTasks = [], userName = Nothing, version = 0, firstName = Nothing}
