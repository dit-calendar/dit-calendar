{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.TelegramLink
    ( TelegramDAO(..), initialUserListState, NewTelegramLink(..), UpdateTelegramLink(..)) where

import           Control.Monad.Reader               (ask)
import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), getOne,
                                                     ixFun, ixSet, (@=))
import           Data.Text                          (Text)

import           Data.Domain.TelegramLink           (TelegramLink (..))
import           Data.Domain.Types                  (EitherResult)

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable TelegramLink where
  empty = ixSet [ ixFun $ \bp -> [ chatId bp ]]

type TelegramLinkList = InterfaceAcid.EntrySet TelegramLink

initialUserListState :: TelegramLinkList
initialUserListState = InterfaceAcid.initialState

-- create a new, empty user and add it to the database
newTelegramLink :: TelegramLink -> Update TelegramLinkList TelegramLink
newTelegramLink = InterfaceAcid.newEntry

updateTelegramLink :: TelegramLink -> Update TelegramLinkList (EitherResult TelegramLink)
updateTelegramLink = InterfaceAcid.updateEntry

$(makeAcidic ''TelegramLinkList ['newTelegramLink, 'updateTelegramLink])

class Monad m => TelegramDAO m where
    create :: NewTelegramLink -> m TelegramLink
    update :: UpdateTelegramLink -> m (EitherResult TelegramLink)
