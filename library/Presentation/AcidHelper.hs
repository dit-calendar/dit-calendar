{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Presentation.AcidHelper ( CtrlV, CtrlV', App, withAcid, Acid ) where

import           Control.Exception                  (bracket)
import           Control.Monad.Reader               (ReaderT, ask)
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

type App = ServerPartT (ReaderT Acid IO)
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response

instance HasAcidState CtrlV' UserAcid.UserList where
    getAcidState = acidUserListState <$> ask

instance HasAcidState CtrlV' CalendarEntryAcid.EntryList where
    getAcidState = acidEntryListState <$> ask

instance HasAcidState CtrlV' TaskAcid.TaskList where
    getAcidState = acidTaskListState <$> ask

instance HasAcidState CtrlV' AuthenticateState where
    getAcidState = acidAuthState <$> ask

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
