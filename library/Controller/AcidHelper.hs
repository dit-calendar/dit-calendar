{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Controller.AcidHelper where

import Prelude              hiding ( head, id )
import System.FilePath      ( (</>) )
import Data.Maybe           ( fromMaybe )
import Control.Exception    ( bracket )

import Data.Acid            ( openLocalStateFrom, AcidState(..) )
import Data.Acid.Local      ( createCheckpointAndClose )
import Happstack.Server     ( Response )
import Happstack.Foundation ( FoundationT, HasAcidState(..), FoundationT', getAcidSt )

import Repository.UserRepo                      as UserRepo
import Repository.Calendar.CalendarEntryRepo    as CalendarEntryRepo
import Route.PageEnum       ( SiteMap )


type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

data Acid = Acid
   {
     acidUserListState         :: AcidState UserRepo.UserList
     , acidEntryListState      :: AcidState CalendarEntryRepo.EntryList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserRepo.UserList where
    getAcidState = acidUserListState <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) CalendarEntryRepo.EntryList where
    getAcidState = acidEntryListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
    in bracket (openLocalStateFrom userPath UserRepo.initialUserListState) createCheckpointAndClose $ \c ->
       bracket (openLocalStateFrom calendarEntryPath CalendarEntryRepo.initialEntryListState) createCheckpointAndClose $ \g ->
        f (Acid c g)