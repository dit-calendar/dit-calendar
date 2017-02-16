module Route.Routing where

import Domain.User
import Controller.AcidHelper
import Controller.UserController
import Controller.HomeController
import Route.PageEnum

-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
    case url of
      HomePage     -> getHomePage
      (UserPage i) -> getUserPage i
