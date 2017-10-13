module Data.Repository.UserRepo
    ( deleteUser, updateName, addCalendarEntryToUser, addTaskToUser
    , deleteCalendarEntryFromUser, deleteTaskFromUser, getUser, createUser ) where

import Prelude hiding ( head )
import Control.Monad.IO.Class
import Data.List                         ( delete )
import Data.Maybe                        ( fromJust )

import Data.Domain.User                  ( User(..) )
import Data.Domain.Types                 ( EntryId, TaskId, UserId )

import qualified Data.Repository.MonadDB.User        as DBRepo
import qualified Data.Repository.Acid.UserAcid       as UserAcid

createUser :: DBRepo.MonadDBUser m => String -> m User
createUser name = let user = User { name = name
                    , userId = undefined
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        DBRepo.create $ UserAcid.NewUser user

deleteUser :: DBRepo.MonadDBUser m => User -> m ()
deleteUser user = DBRepo.delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: DBRepo.MonadDBUser m => User -> m ()
updateUser user = DBRepo.update $ UserAcid.UpdateUser user

updateName :: DBRepo.MonadDBUser m => User -> String -> m ()
updateName user newName = updateUser user {name = newName}

addCalendarEntryToUser :: DBRepo.MonadDBUser m => User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    updateUser user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUser :: DBRepo.MonadDBUser m =>
                            User -> EntryId -> m ()
deleteCalendarEntryFromUser user entryId =
    updateUser user {calendarEntries = delete entryId (calendarEntries user)}

addTaskToUser :: DBRepo.MonadDBUser m => TaskId -> User -> m ()
addTaskToUser taskId user =
    updateUser user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUser :: DBRepo.MonadDBUser m => TaskId -> User -> m ()
deleteTaskFromUser taskId user =
    updateUser user {belongingTasks = delete taskId (belongingTasks user)}

getUser :: (DBRepo.MonadDBUser m, MonadIO m) => UserId -> m User
getUser userId = do
    mUser <- DBRepo.query $ UserAcid.UserById userId
    return $ fromJust mUser