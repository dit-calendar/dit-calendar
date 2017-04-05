module Controller.UserController where

import Happstack.Server         ( ok, toResponse, lookRead 
                                , Method(GET), method)
import Happstack.Foundation     ( query, update )

import Domain.User              as User      ( User(..) )
import Repository.UserRepo      as UserRepo
import Controller.AcidHelper    ( CtrlV )

--handler for userPage
userPage :: Int -> CtrlV
userPage i =
    do
       mUser <- query (UserRepo.UserById i)
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
       userList <- query UserRepo.AllUsers
       case userList of
            [] ->
                ok $ toResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                ok $ toResponse $ temp ++ printUsersList (x:xs)

createUser :: String -> CtrlV
createUser name =
    do
        mUser <- update (UserRepo.NewUser name)
        ok $ toResponse $ "User created: " ++ show mUser

printUsersList :: [User] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs
