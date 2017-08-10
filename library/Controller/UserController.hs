module Controller.UserController where

import Happstack.Server         ( ok, toResponse, Method(GET), method)
import Happstack.Foundation     ( query )

import Data.Domain.User                      as User      ( User(..))
import Data.Domain.Types             ( UserId, EntryId )

import qualified Data.Repository.Acid.UserAcid         as UserAcid
import qualified Data.Repository.UserRepo              as UserRepo
import qualified Data.Repository.UserRepoHelper        as UserRepoHelper

import Controller.AcidHelper    ( CtrlV )

--handler for userPage
userPage :: UserId -> CtrlV
userPage i =
    do
       mUser <- query (UserAcid.UserById i)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the name and saw: " ++ show u

--handler for userPage
usersPage :: CtrlV
usersPage =
    let temp = "Anzeige aller User \n" in
    do method GET
       userList <- query UserAcid.AllUsers
       case userList of
            [] ->
                ok $ toResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                ok $ toResponse $ temp ++ printUsersList (x:xs)

createUser :: String -> CtrlV
createUser name =
    do
        mUser <- UserRepo.createUser name
        ok $ toResponse $ "User created: " ++ show mUser

updateUser :: UserId -> String -> CtrlV
updateUser id name =
    do
       mUser <- query (UserAcid.UserById id)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show id
            (Just u) -> do
                 UserRepo.updateName u name
                 ok $ toResponse $ "User with id:" ++ show id ++ "updated"

deleteUser :: UserId -> CtrlV
deleteUser i = do
    mUser <- query (UserAcid.UserById i)
    case mUser of
        Nothing ->
            ok $ toResponse $ "Could not find a user with id " ++ show i
        (Just u) -> do
            UserRepoHelper.deleteUser u
            ok $ toResponse $ "User with id:" ++ show i ++ "deleted"

printUsersList :: [User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs
