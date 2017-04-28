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

import Data.Repository.Acid.UserAcid          as UserAcid
import Data.Repository.Acid.CalendarAcid      as CalendarAcid
import Route.PageEnum       ( SiteMap )


type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

data Acid = Acid
   {
     acidUserListState         :: AcidState UserAcid.UserList
     , acidEntryListState      :: AcidState CalendarAcid.EntryList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserAcid.UserList where
    getAcidState = acidUserListState <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) CalendarAcid.EntryList where
    getAcidState = acidEntryListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarEntryPath = basePath </> "entryList"
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \c ->
       bracket (openLocalStateFrom calendarEntryPath CalendarAcid.initialEntryListState) createCheckpointAndClose $ \g ->
        f (Acid c g)