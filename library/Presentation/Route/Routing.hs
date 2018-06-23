module Presentation.Route.Routing ( authOrRoute ) where

import           Data.Acid                              (AcidState)
import           Happstack.Authenticate.Core            (AuthenticateState,
                                                         AuthenticateURL (..))
import           Happstack.Server                       (BodyPolicy (..),
                                                         Response, ServerPartT,
                                                         decodeBody,
                                                         defaultBodyPolicy)
import           Web.Routes                             (RouteT, mapRouteT,
                                                         nestURL)

import           Presentation.AcidHelper                (CtrlV)
import           Presentation.HttpServerHelper          (mapServerPartTIO2App)
import           Presentation.Route.CalendarRoute       (routeCalendarEntry)
import           Presentation.Route.PageEnum            (Sitemap (..))
import           Presentation.Route.TaskRoute           (routeTask,
                                                         routeTaskWithCalendar,
                                                         routeTaskWithUser)
import           Presentation.Route.UserRoute           (routeDetailUser,
                                                         routeUser)

import qualified Presentation.Controller.HomeController as HomeController
import qualified Presentation.Controller.UserController as UserController


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

--authenticate or route
authOrRoute :: AcidState AuthenticateState
        -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
        -> Sitemap -> CtrlV
authOrRoute authenticateState routeAuthenticate url = case url of
    Authenticate authenticateURL ->
        if show authenticateURL ==  "AuthenticationMethods (Just (AuthenticationMethod {_unAuthenticationMethod = \"password\"},[\"account\"]))"
        then UserController.createUser authenticateURL routeAuthenticate
        else mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
    other -> route other authenticateState

-- | the route mapping function
route :: Sitemap -> AcidState AuthenticateState -> CtrlV
route url authState =
    do  decodeBody myPolicy
        case url of
            Home                 -> HomeController.homePage
            Userdetail           -> routeDetailUser
            User i               -> routeUser i authState
            CalendarEntry i      -> routeCalendarEntry i authState
            Task i               -> routeTask i authState
            TaskWithCalendar e u -> routeTaskWithCalendar e u authState
            TaskWithUser t u     -> routeTaskWithUser t u authState

