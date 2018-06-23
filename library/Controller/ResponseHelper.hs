module Controller.ResponseHelper
    ( userExist, entryExist, taskExist, okResponse ) where

import Happstack.Server         ( ok, toResponse )

import Controller.AcidHelper         ( CtrlV )
import Data.Domain.Types             ( UserId, EntryId, TaskId )
import Data.Domain.User              ( User )
import Data.Domain.Task              ( Task )
import Data.Domain.CalendarEntry     ( CalendarEntry )


userExist :: UserId -> (User -> CtrlV) -> Maybe User -> CtrlV
userExist i = onNothing $ "Could not find a user with id " ++ show i

entryExist :: EntryId -> (CalendarEntry -> CtrlV) -> Maybe CalendarEntry -> CtrlV
entryExist i = onNothing $ "Could not find a entry with id " ++ show i

taskExist :: TaskId -> (Task -> CtrlV) -> Maybe Task -> CtrlV
taskExist i = onNothing $ "Could not find a task with id " ++ show i

onNothing :: String -> (a -> CtrlV) -> Maybe a -> CtrlV
onNothing message = maybe (okResponse message)

okResponse :: String -> CtrlV
okResponse message = ok $ toResponse message