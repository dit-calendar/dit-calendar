module Controller.HomeController where

import Controller.UserController as UserController
import Controller.AcidHelper

import Happstack.Server  ( Response )

--handler for homePage
getHomePage :: CtrlV Response
getHomePage = UserController.getUsersPage
