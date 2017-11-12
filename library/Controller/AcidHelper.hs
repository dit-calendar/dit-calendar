{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Controller.AcidHelper ( CtrlV, withAcid, Acid ) where

import Prelude
import System.FilePath      ( (</>) )
import Data.Maybe           ( fromMaybe )
import Control.Exception    ( bracket )

import Data.Acid            ( openLocalStateFrom, AcidState(..) )
import Data.Acid.Local      ( createCheckpointAndClose )
import Happstack.Server     ( Response )
import Happstack.Foundation ( FoundationT, HasAcidState(..), FoundationT', getAcidSt )

import qualified Data.Repository.Acid.UserAcid          as UserAcid
import qualified Data.Repository.Acid.CalendarAcid      as CalendarAcid
import qualified Data.Repository.Acid.TaskAcid          as TaskAcid
import Route.PageEnum       ( SiteMap )


type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

data Acid = Acid
   {
     acidUserListState         :: AcidState UserAcid.UserList
     , acidEntryListState      :: AcidState CalendarAcid.EntryList
     , acidTaskListState      :: AcidState TaskAcid.TaskList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserAcid.UserList where
    getAcidState = acidUserListState <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) CalendarAcid.EntryList where
    getAcidState = acidEntryListState <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) TaskAcid.TaskList where
    getAcidState = acidTaskListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
        taskPath = basePath </> "taskList"
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \c ->
       bracket (openLocalStateFrom calendarEntryPath CalendarAcid.initialEntryListState) createCheckpointAndClose $ \g ->
       bracket (openLocalStateFrom taskPath TaskAcid.initialTaskListState) createCheckpointAndClose $ \t ->
        f (Acid c g t)