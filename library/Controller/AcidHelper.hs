{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses,
    OverloadedStrings, ScopedTypeVariables, TypeFamilies,
    FlexibleInstances #-}

module Controller.AcidHelper ( CtrlV, CtrlV', App, withAcid, Acid ) where

import Prelude
import System.FilePath      ( (</>) )
import Data.Maybe           ( fromMaybe )
import Control.Exception    ( bracket )
import Control.Monad.Reader ( ReaderT, ask )

import Data.Acid            ( openLocalStateFrom, AcidState(..) )
import Data.Acid.Local      ( createCheckpointAndClose )
import Web.Routes           ( RouteT )
import Happstack.Server     ( Response, ServerPartT )
import Happstack.Foundation ( HasAcidState(..) )

import Route.PageEnum            ( Sitemap )

import qualified Data.Repository.Acid.User              as UserAcid
import qualified Data.Repository.Acid.CalendarEntry     as CalendarEntryAcid
import qualified Data.Repository.Acid.Task              as TaskAcid


data Acid = Acid
   {
     acidUserListState         :: AcidState UserAcid.UserList
     , acidEntryListState      :: AcidState CalendarEntryAcid.EntryList
     , acidTaskListState       :: AcidState TaskAcid.TaskList
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

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
        taskPath = basePath </> "taskList"
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \c ->
       bracket (openLocalStateFrom calendarEntryPath CalendarEntryAcid.initialEntryListState) createCheckpointAndClose $ \g ->
       bracket (openLocalStateFrom taskPath TaskAcid.initialTaskListState) createCheckpointAndClose $ \t ->
        f (Acid c g t)
