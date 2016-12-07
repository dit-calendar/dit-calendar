module Controller.UserController where

import Route.PageEnum as Page
import Domain.User as User

import Prelude                 hiding ( head )

import Happstack.Server  ( Response, ServerPartT
                          , ok, toResponse, lookRead )
import Web.Routes        ( RouteT, runRouteT, Site(..)
                          , setDefault, mkSitePI )
import Web.Routes.Happstack    ( implSite )
import Data.Acid            ( AcidState )
import Data.Acid.Advanced   ( query', update' )

--handler for userPage
getUserPage :: AcidState User.UserList -> Integer -> RouteT SiteMap (ServerPartT IO) Response
getUserPage acid i =
    do  mUser <- query' acid (UserById i)
        case mUser of
            Nothing ->
                ok $ toResponse $ "Could not find a user with id " ++ show i
            (Just u) ->
                ok $ toResponse $ "peeked at the name and saw: " ++ show (User.userId u)

--handler for userPage
getUsersPage :: AcidState User.UserList -> RouteT SiteMap (ServerPartT IO) Response
getUsersPage acid =
    let temp = "Anzeige aller User \n" in
    do  mUser <- query' acid AllUsers
        case mUser of
            [] ->
                ok $ toResponse (temp ++ "Liste ist leer")
            (x:xs) ->
                ok $ toResponse $ temp ++ printUsersList (x:xs)

printUsersList :: [User.UserState] -> String
printUsersList l = case l of
    --schlechte implementierung. es gibt dafür schon fertige funktionen (annonyme funktion uergeben)
    []     -> ""
    (x:xs) -> ("User: " ++ User.name x ++ "mit Id: "++ show (User.userId x))
        ++ "\n" ++ printUsersList xs
