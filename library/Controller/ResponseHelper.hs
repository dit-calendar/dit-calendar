module Controller.ResponseHelper
    ( onUserExist, onEntryExist, onTaskExist, okResponse ) where

import Happstack.Server         ( ok, toResponse )
import Happstack.Foundation     ( query )

import Controller.AcidHelper         ( CtrlV )
import Data.Domain.Types             ( UserId, EntryId, TaskId )
import Data.Domain.User              ( User )
import Data.Domain.Task              ( Task )
import Data.Domain.CalendarEntry     ( CalendarEntry )

import qualified Data.Repository.Acid.User             as UserAcid
import qualified Data.Repository.Acid.CalendarEntry    as CalendarEntryAcid
import qualified Data.Repository.Acid.Task             as TaskAcid


onUserExist :: UserId -> (User -> CtrlV) -> CtrlV
onUserExist i daoFunction = query (UserAcid.UserById i) >>= (onNothing $ "Could not find a user with id " ++ show i) daoFunction

onEntryExist :: EntryId -> (CalendarEntry -> CtrlV) -> CtrlV
onEntryExist i daoFunction = query (CalendarEntryAcid.EntryById i) >>= (onNothing $ "Could not find a entry with id " ++ show i) daoFunction

onTaskExist :: TaskId -> (Task -> CtrlV) -> CtrlV
onTaskExist i daoFunction = query (TaskAcid.TaskById i) >>= (onNothing $ "Could not find a task with id " ++ show i) daoFunction

onNothing :: String -> (a -> CtrlV) -> Maybe a -> CtrlV
onNothing message = maybe (okResponse message)

okResponse :: String -> CtrlV
okResponse message = ok $ toResponse message