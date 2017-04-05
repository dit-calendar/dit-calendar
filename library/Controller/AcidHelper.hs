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

import Repository.UserRepo        as UserRepo
import Repository.CalendarRepo    as CalendarRepo
import Route.PageEnum       ( SiteMap )


type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

data Acid = Acid
   {
     acidUserListState         :: AcidState UserRepo.UserList
     , acidCalendarListState   :: AcidState CalendarRepo.CalendarList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserRepo.UserList where
    getAcidState = acidUserListState <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) CalendarRepo.CalendarList where
    getAcidState = acidCalendarListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        userPath = basePath </> "userList"
        calendarPath = basePath </> "calendarList"
    in bracket (openLocalStateFrom userPath UserRepo.initialUserListState) createCheckpointAndClose $ \c ->
       bracket (openLocalStateFrom calendarPath CalendarRepo.initialCalendarListState) createCheckpointAndClose $ \g ->
        f (Acid c g)