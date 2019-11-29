module Data.Repository.PermissionControl (executeUnderUserPermission) where

import           Data.Domain.Types (EitherResult, Entity,
                                    ResultError (PermissionAccessInsufficient),
                                    getId, getUsersAccessRestriction)
import           Data.Domain.User  (User)

isUserAllowed :: Entity a => User -> a-> Bool
isUserAllowed user entity =
    let allowedUser = getUsersAccessRestriction entity in
        (null allowedUser || (getId user `elem` allowedUser))

executeUnderUserPermission :: (Entity a, Monad m) => User -> a -> m (EitherResult b) -> m (EitherResult b)
executeUnderUserPermission user entity = executeUnderPermission (isUserAllowed user entity)

executeUnderPermission :: Monad m => Bool -> m (EitherResult b) -> m (EitherResult b)
executeUnderPermission executionAllowed execution =
    if executionAllowed
        then return $ Left PermissionAccessInsufficient
        else execution
