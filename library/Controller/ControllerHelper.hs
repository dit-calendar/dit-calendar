module Controller.ControllerHelper
    ( userExist, entryExist, taskExist, okResponse ) where

import Happstack.Server         ( ok, toResponse )

import Controller.AcidHelper         ( CtrlV )


userExist :: Show i => i -> (a -> CtrlV) -> Maybe a -> CtrlV
userExist i = onNothing $ "Could not find a user with id " ++ show i

entryExist :: Show i => i -> (a -> CtrlV) -> Maybe a -> CtrlV
entryExist i = onNothing $ "Could not find a entry with id " ++ show i

taskExist :: Show i => i -> (a -> CtrlV) -> Maybe a -> CtrlV
taskExist i = onNothing $ "Could not find a task with id " ++ show i

onNothing :: String -> (a -> CtrlV) -> Maybe a -> CtrlV
onNothing message = maybe (okResponse message)

okResponse :: String -> CtrlV
okResponse message = ok $ toResponse message