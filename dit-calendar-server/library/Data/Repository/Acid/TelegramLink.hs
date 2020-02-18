{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.TelegramLink
    ( TelegramDAO(..), initialTelegramState, TelegramLinkList, TelegramLinkById(..), NewTelegramLink(..), UpdateTelegramLink(..)) where

import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), ixFun,
                                                     ixSet)

import           Data.Domain.TelegramLink           (TelegramLink (..))
import           Data.Domain.Types                  (EitherResult,
                                                     TelegramChatId)

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable TelegramLink where
  empty = ixSet [ ixFun $ \bp -> [ chatId bp ]]

type TelegramLinkList = InterfaceAcid.EntrySet TelegramLink

initialTelegramState :: TelegramLinkList
initialTelegramState = InterfaceAcid.initialState

telegramLinkById :: TelegramChatId -> Query TelegramLinkList (Maybe TelegramLink)
telegramLinkById = InterfaceAcid.entryById

-- create a new, empty user and add it to the database
newTelegramLink :: TelegramLink -> Update TelegramLinkList TelegramLink
newTelegramLink = InterfaceAcid.newEntry

updateTelegramLink :: TelegramLink -> Update TelegramLinkList (EitherResult TelegramLink)
updateTelegramLink = InterfaceAcid.updateEntry

$(makeAcidic ''TelegramLinkList ['telegramLinkById, 'newTelegramLink, 'updateTelegramLink])

class Monad m => TelegramDAO m where
    create :: NewTelegramLink -> m TelegramLink
    update :: UpdateTelegramLink -> m (EitherResult TelegramLink)
    query  :: TelegramLinkById -> m (Maybe TelegramLink)
