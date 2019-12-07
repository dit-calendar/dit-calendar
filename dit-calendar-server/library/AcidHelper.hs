{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module AcidHelper ( CtrlV, App, withAcid, Acid, AppContext(..), setCurrentUser, getCurrentUser ) where

import           Control.Exception                  (bracket)
import           Control.Monad.Reader               (ReaderT, asks, local)
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

import           HappstackHelper                    (FoundationT)
import           Presentation.Route.PageEnum        (Sitemap)

import qualified Data.Domain.User                   as DomainUser
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

data AppContext = AppContext
    {
    acidState     :: Acid
    , currentUser :: Maybe DomainUser.User
    }

updateUserInAppContext :: DomainUser.User -> AppContext -> AppContext
updateUserInAppContext mUser reader = reader { currentUser = Just mUser}

setCurrentUser :: DomainUser.User -> App m -> App m
setCurrentUser user = local (updateUserInAppContext user)

getCurrentUser :: App (Maybe DomainUser.User)
getCurrentUser = asks currentUser

type App  = FoundationT AppContext ()
type CtrlV'   = RouteT Sitemap App
type CtrlV    = CtrlV' Response

instance HasAcidState App UserAcid.UserList where
    getAcidState = asks (acidUserListState . acidState)

instance HasAcidState App CalendarEntryAcid.EntryList where
    getAcidState = asks (acidEntryListState . acidState)

instance HasAcidState App TaskAcid.TaskList where
    getAcidState = asks (acidTaskListState . acidState)

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
    in bracket (openLocalStateFrom userPath UserAcid.initialUserListState) createCheckpointAndClose $ \user ->
       bracket (openLocalStateFrom calendarEntryPath CalendarEntryAcid.initialEntryListState) createCheckpointAndClose $ \calend ->
       bracket (openLocalStateFrom taskPath TaskAcid.initialTaskListState) createCheckpointAndClose $ \task ->
        f (Acid user calend task authState)
