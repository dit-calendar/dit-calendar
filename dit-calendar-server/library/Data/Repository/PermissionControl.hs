module Data.Repository.PermissionControl (executeUnderUserPermission) where

import           Data.Domain.Types (EitherResult, Entity, getId,
                                    getUsersAccessRestriction)
import           Data.Domain.User  (User)

isUserAllowed :: Entity a => User -> a-> Bool
isUserAllowed user entity =
    let allowedUser = getUsersAccessRestriction entity
     in (null allowedUser || (getId user `elem` allowedUser))

executeUnderUserPermission :: (Entity a, Monad m) => User -> a -> m (EitherResult b) -> m (EitherResult b)
executeUnderUserPermission user entry = executeUnderPermission (isUserAllowed user entry) entry

executeUnderPermission :: (Entity a, Monad m) => Bool -> a -> m (EitherResult b) -> m (EitherResult b)
executeUnderPermission = undefined
