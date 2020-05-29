module Data.Repository.PermissionControl (executeUnderUserPermission) where

import           AppContext        (App, AppContext (..), getCurrentUser)
import           Data.Domain.Types (EitherResult, Entity,
                                    ResultError (PermissionAccessInsufficient),
                                    getId, getUsersAccessRestriction)
import           Data.Domain.User  (User)

isUserAllowed :: Entity a key => User -> a-> Bool
isUserAllowed user entity =
    let allowedUser = getUsersAccessRestriction entity in
        (null allowedUser || (getId user `elem` allowedUser))

executeUnderUserPermission :: (Entity a key, AppContext m) => a -> m (EitherResult a) -> m (EitherResult a)
executeUnderUserPermission entity f = do
    mUser <- getCurrentUser
    case mUser of
        Just user -> executeUnderPermission (isUserAllowed user entity) f
        Nothing   -> return $ Left PermissionAccessInsufficient

executeUnderPermission :: Monad m => Bool -> m (EitherResult b) -> m (EitherResult b)
executeUnderPermission executionAllowed f =
    if executionAllowed
        then f
        else return $ Left PermissionAccessInsufficient
