module Controller.UserController where

import           Happstack.Foundation      (query)
import           Happstack.Server          (Method (GET), method)

import           Controller.AcidHelper     (CtrlV)
import           Controller.ResponseHelper (okResponse, onUserExist)
import           Data.Domain.Types         (UserId)
import           Data.Domain.User          as User (User (..))

import qualified Data.Repository.Acid.User as UserAcid
import qualified Data.Repository.UserRepo  as UserRepo
import qualified Data.Service.User         as UserService


--handler for userPage
userPage :: UserId -> CtrlV
userPage i = onUserExist i (\u -> okResponse $ "peeked at the name and saw: " ++ show u)

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

updateUser :: UserId -> String -> User -> CtrlV
updateUser id name loggedUser = onUserExist id updateUsr
    where updateUsr user = do
              UserRepo.updateName user name
              okResponse $ "User with id:" ++ show id ++ "updated"

deleteUser :: UserId -> User -> CtrlV
deleteUser i loggedUser = onUserExist i deleteUsr
    where deleteUsr user = do
              UserService.deleteUser user
              okResponse $ "User with id:" ++ show i ++ "deleted"

printUsersList :: [User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs


