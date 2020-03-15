{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Server.AcidInitializer ( withAcid ) where

import           Control.Exception                  (bracket)
import           Control.Monad.Reader               (asks)
import           Data.Maybe                         (fromMaybe)
import           Prelude
import           System.FilePath                    ((</>))

import           AppContext                         (App, AppReader (acidState))
import           Data.Acid                          (AcidState (..),
                                                     openLocalStateFrom)
import           Data.Acid.Local                    (createCheckpointAndClose)
import           Happstack.Authenticate.Core        (AuthenticateState)
import           Server.DBState                     (Acid (..))

import           Server.HappstackHelper             (HasAcidState (..))

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.TelegramLink  as TelegramAcid
import qualified Data.Repository.Acid.User          as UserAcid


instance HasAcidState App UserAcid.UserList where
    getAcidState = asks (acidUserListState . acidState)

instance HasAcidState App CalendarEntryAcid.EntryList where
    getAcidState = asks (acidEntryListState . acidState)

instance HasAcidState App TaskAcid.TaskList where
    getAcidState = asks (acidTaskListState . acidState)

instance HasAcidState App TelegramAcid.TelegramLinkList where
    getAcidState = asks (acidTelegramLinkState . acidState)

instance HasAcidState App AuthenticateState where
    getAcidState = asks (acidAuthState . acidState)

withAcid :: AcidState AuthenticateState -> Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid authState mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
        taskPath = basePath </> "taskList"
        telegramPath = basePath </> "telegramList"
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \user ->
       bracket (openLocalStateFrom calendarEntryPath CalendarEntryAcid.initialEntryListState) createCheckpointAndClose $ \calend ->
       bracket (openLocalStateFrom taskPath TaskAcid.initialTaskListState) createCheckpointAndClose $ \task ->
       bracket (openLocalStateFrom telegramPath TelegramAcid.initialTelegramState) createCheckpointAndClose $ \telegram ->
        f (Acid user calend task telegram authState)
