module Presentation.Route.Routing
    ( authOrRoute
    ) where

import           Happstack.Authenticate.Core            (AuthenticateURL (..))
import           Happstack.Foundation                   (lift)
import           Happstack.Server                       (BodyPolicy (..),
                                                         Response, ServerPartT,
                                                         decodeBody,
                                                         defaultBodyPolicy)
import           Web.Routes                             (RouteT, mapRouteT,
                                                         nestURL)

import           Presentation.AcidHelper                (App, CtrlV)
import           Presentation.HttpServerHelper          (mapServerPartTIO2App)
import           Presentation.Route.CalendarRoute       (routeCalendarEntry)
import           Presentation.Route.PageEnum            (Sitemap (..))
import           Presentation.Route.TaskRoute           (routeTask,
                                                         routeTaskWithCalendar,
                                                         routeTaskWithUser)
import           Presentation.Route.UserRoute           (routeDetailUser,
                                                         routeUser, routeUsers)

import qualified Presentation.Controller.HomeController as HomeController
import qualified Presentation.Controller.UserController as UserController

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

--authenticate or route
authOrRoute :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> Sitemap -> CtrlV
authOrRoute routeAuthenticate url =
    case url of
        Authenticate authenticateURL ->
            if show authenticateURL ==
               "AuthenticationMethods (Just (AuthenticationMethod {_unAuthenticationMethod = \"password\"},[\"account\"]))"
                then lift $ UserController.createUser authenticateURL routeAuthenticate
                else mapRouteT mapServerPartTIO2App $ nestURL Authenticate $ routeAuthenticate authenticateURL
        other -> lift $ route other

-- | the route mapping function
route :: Sitemap -> App Response
route url = do
    decodeBody myPolicy
    case url of
        Home                 -> HomeController.homePage
        Userdetail           -> routeDetailUser
        User i               -> routeUser i
        Users                -> routeUsers
        CalendarEntry i      -> routeCalendarEntry i
        Task i               -> routeTask i
        TaskWithCalendar e u -> routeTaskWithCalendar e u
        TaskWithUser t u     -> routeTaskWithUser t u
