module Data.Repository.PermissionControl (executeUnderUserPermission) where

import           Data.Domain.Types (EitherResult, Entry, getId,
                                    getUsersAccessRestriction)
import           Data.Domain.User  (User)

isUserAllowed :: Entry a => User -> a-> Bool
isUserAllowed user entity =
    let allowedUser = getUsersAccessRestriction entity
     in (null allowedUser || (getId user `elem` allowedUser))

executeUnderUserPermission :: (Entry a, Monad m) => User -> a -> m (EitherResult b) -> m (EitherResult b)
executeUnderUserPermission user entry = executeUnderPermission (isUserAllowed user entry) entry

executeUnderPermission :: (Entry a, Monad m) => Bool -> a -> m (EitherResult b) -> m (EitherResult b)
executeUnderPermission = undefined
