module Route.Routing where

import Domain.User
import Controller.AcidHelper
import Controller.UserController
import Controller.HomeController
import Route.PageEnum


import Prelude                 hiding ( head )

import Happstack.Server  ( Response )


-- | the route mapping function
route :: SiteMap -> CtrlV Response
route url =
    case url of
      HomePage     -> getHomePage
      (UserPage i) -> getUserPage i
