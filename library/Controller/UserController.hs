module Controller.UserController where

import Domain.User as User
import Controller.Repo as Repo

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse, lookRead )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Happstack.Foundation

--handler for userPage
getUserPage :: Integer -> CtrlV Response
getUserPage i =
    do
       mUser <- query (User.UserById i)
       case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the name and saw: " ++ show (User.userId u)

--handler for userPage
getUsersPage :: CtrlV Response
getUsersPage =
    let temp = "Anzeige aller User \n" in
    do method GET
       allUsers <- query AllUsers
       case allUsers of
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
