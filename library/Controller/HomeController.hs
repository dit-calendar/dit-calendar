module Controller.HomeController where

import Controller.UserController as UserController
import Controller.Repo

import Happstack.Server  ( Response )

--handler for homePage
getHomePage :: CtrlV Response
getHomePage = UserController.getUsersPage
