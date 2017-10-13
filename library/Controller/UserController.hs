module Controller.UserController where

import Happstack.Server         ( Method(GET), method)
import Happstack.Foundation     ( query )

import Controller.ControllerHelper   ( userExist, okResponse )
import Data.Domain.User                      as User      ( User(..))
import Data.Domain.Types             ( UserId )
import Controller.AcidHelper         ( CtrlV )

import qualified Data.Repository.Acid.UserAcid         as UserAcid
import qualified Data.Repository.UserRepo              as UserRepo
import qualified Data.Repository.UserRepoHelper        as UserRepoHelper


--handler for userPage
userPage :: UserId -> CtrlV
userPage i = do
    mUser <- query (UserAcid.UserById i)
    userExist i (\u -> okResponse $ "peeked at the name and saw: " ++ show u) mUser

--handler for userPage
usersPage :: CtrlV
usersPage =
    let temp = "Anzeige aller User \n" in
    do  method GET
        userList <- query UserAcid.AllUsers
        case userList of
            [] ->
                okResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                okResponse $ temp ++ printUsersList (x:xs)

createUser :: String -> CtrlV
createUser name = do
    mUser <- UserRepo.createUser name
    okResponse $ "User created: " ++ show mUser

updateUser :: UserId -> String -> CtrlV
updateUser id name = do
    mUser <- query (UserAcid.UserById id)
    userExist id (\u -> do
            UserRepo.updateName u name
            okResponse $ "User with id:" ++ show id ++ "updated") mUser

deleteUser :: UserId -> CtrlV
deleteUser i = do
    mUser <- query (UserAcid.UserById i)
    userExist i (\u -> do
            UserRepoHelper.deleteUser u
            okResponse $ "User with id:" ++ show i ++ "deleted") mUser

printUsersList :: [User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs
