{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module AcidHelper ( CtrlV, App, withAcid, Acid ) where

import           Control.Exception                  (bracket)
import           Control.Monad.Reader               (ReaderT, asks)
import           Data.Maybe                         (fromMaybe)
import           Prelude
import           System.FilePath                    ((</>))

import           Data.Acid                          (AcidState (..),
                                                     openLocalStateFrom)
import           Data.Acid.Local                    (createCheckpointAndClose)
import           Happstack.Authenticate.Core        (AuthenticateState)
import           Happstack.Foundation               (HasAcidState (..))
import           Happstack.Server                   (Response, ServerPartT)
import           Web.Routes                         (RouteT)

import           HappstackHelper                    (FoundationT, getAcidSt)
import           Presentation.Route.PageEnum        (Sitemap)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid
import qualified Data.Repository.Acid.Task          as TaskAcid
import qualified Data.Repository.Acid.User          as UserAcid


data Acid = Acid
   {
     acidUserListState    :: AcidState UserAcid.UserList
     , acidEntryListState :: AcidState CalendarEntryAcid.EntryList
     , acidTaskListState  :: AcidState TaskAcid.TaskList
     , acidAuthState      :: AcidState AuthenticateState
   }

type App  = FoundationT Acid ()
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response

instance HasAcidState App UserAcid.UserList where
    getAcidState = acidUserListState <$> getAcidSt

instance HasAcidState App CalendarEntryAcid.EntryList where
    getAcidState = acidEntryListState <$> getAcidSt

instance HasAcidState App TaskAcid.TaskList where
    getAcidState = acidTaskListState <$> getAcidSt

instance HasAcidState App AuthenticateState where
    getAcidState = acidAuthState <$> getAcidSt

withAcid :: AcidState AuthenticateState -> Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid authState mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
        taskPath = basePath </> "taskList"
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \user ->
       bracket (openLocalStateFrom calendarEntryPath CalendarEntryAcid.initialEntryListState) createCheckpointAndClose $ \calend ->
       bracket (openLocalStateFrom taskPath TaskAcid.initialTaskListState) createCheckpointAndClose $ \task ->
        f (Acid user calend task authState)
