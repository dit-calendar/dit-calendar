module Route.Routing where

import Controller.AcidHelper     ( CtrlV )
import Controller.UserController ( userPage )
import Controller.HomeController ( homePage )
import Route.PageEnum            ( SiteMap(..) )


-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
    case url of
      Home     -> homePage
      (User i) -> userPage i
