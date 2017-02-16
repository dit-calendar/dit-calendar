module Controller.UserController where

import Domain.User as User
import Repository.UserRepository as UserRepo
import Controller.AcidHelper
import Happstack.Server  ( ok, toResponse, lookRead 
                           , Method(GET), method)
import Happstack.Foundation (query)

--handler for userPage
getUserPage :: Integer -> CtrlV
getUserPage i =
    do
       mUser <- query (UserRepo.UserById i)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the name and saw: " ++ show (User.userId u)

--handler for userPage
getUsersPage :: CtrlV
getUsersPage =
    let temp = "Anzeige aller User \n" in
    do method GET
       userList <- query UserRepo.AllUsers
       case userList of
            [] ->
                ok $ toResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                ok $ toResponse $ temp ++ printUsersList (x:xs)

printUsersList :: [User.UserState] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uebergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs
