{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.TelegramLink
    ( TelegramDAO(..), initialTelegramState, TelegramLinkList, TelegramLinkById(..), NewTelegramLink(..), UpdateTelegramLink(..)) where

import           Control.Monad.State                (get, put)

import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), insert,
                                                     ixFun, ixSet)

import           Data.Domain.TelegramLink           (TelegramLink (..))
import           Data.Domain.Types                  (EitherResult,
                                                     TelegramChatId, UserId,
                                                     setVersion)

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable TelegramLink where
  empty = ixSet [ ixFun $ \bp -> [ (chatId bp, owner bp) ]]

type TelegramLinkList = InterfaceAcid.EntrySet TelegramLink (TelegramChatId, UserId)

initialTelegramState :: TelegramLinkList
initialTelegramState = InterfaceAcid.initialState (0,0) --don't use the key

telegramLinkById :: (TelegramChatId, UserId) -> Query TelegramLinkList (Maybe TelegramLink)
telegramLinkById = InterfaceAcid.entryById

-- create a new, empty user and add it to the database
newTelegramLink :: TelegramLink -> Update TelegramLinkList TelegramLink
newTelegramLink entry =
    do  b@InterfaceAcid.EntrySet{..} <- get
        put b { InterfaceAcid.entrys      = insert (setVersion entry 0) entrys
              }
        return entry

updateTelegramLink :: TelegramLink -> Update TelegramLinkList (EitherResult TelegramLink)
updateTelegramLink = InterfaceAcid.updateEntry

$(makeAcidic ''TelegramLinkList ['telegramLinkById, 'newTelegramLink, 'updateTelegramLink])

class Monad m => TelegramDAO m where
    create :: NewTelegramLink -> m TelegramLink
    update :: UpdateTelegramLink -> m (EitherResult TelegramLink)
    query  :: TelegramLinkById -> m (Maybe TelegramLink)
