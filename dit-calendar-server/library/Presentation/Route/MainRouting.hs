module Presentation.Route.MainRouting
    ( routeWithOptions
    ) where

import           Control.Monad.Cont                       (lift)

import           Happstack.Authenticate.Core              (AuthenticateURL (..))
import           Happstack.Server                         (BodyPolicy (..),
                                                           Method (OPTIONS),
                                                           Response,
                                                           ServerPartT,
                                                           decodeBody,
                                                           defaultBodyPolicy)
import           Web.Routes                               (RouteT, mapRouteT,
                                                           nestURL)

import           AppContext                               (App, CtrlV)
import           Conf.Config                              (Config (..))
import           Presentation.Route.CalendarRoute         (routeCalendarEntry, routeCalendarEntryDetails,
                                                           routeCalendarFilter)
import           Presentation.Route.PageEnum              (Sitemap (..))
import           Presentation.Route.TaskRoute             (routeTask,
                                                           routeTaskDetail,
                                                           routeTaskWithUser)
import           Presentation.Route.UserRoute             (routeDetailUser,
                                                           routeUser,
                                                           routeUsers)
import           Server.HappstackHelper                   (liftServerPartT2FoundationT)
import           Server.HttpServerHelper                  (getHttpMethod)
import           Server.ResponseBuilder                   (addCorsToResponse,
                                                           corsResponse)

import qualified Presentation.Controller.HomeController   as HomeController
import qualified Presentation.Controller.LogoutController as LogoutController
import qualified Presentation.Controller.UserController   as UserController

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
                else mapRouteT liftServerPartT2FoundationT $ nestURL Authenticate $ routeAuthenticate authenticateURL
        other -> lift $ route other

-- | the route mapping function
route :: Sitemap -> App Response
route url = do
    decodeBody myPolicy
    case url of
        Home                       -> HomeController.homePage
        Logout                     -> LogoutController.logout
        Userdetail                 -> routeDetailUser
        User i                     -> routeUser i
        Users                      -> routeUsers
        -- calendar routing
        CalendarEntry              -> routeCalendarEntry
        CalendarFilter             -> routeCalendarFilter
        CalendarEntryDetail eId    -> routeCalendarEntryDetails eId
        -- task routing
        CalendarTask eId           -> routeTask eId
        CalendarTaskDetail eId tId -> routeTaskDetail eId tId
        TaskWithUser eId tId       -> routeTaskWithUser eId tId

routeWithOptions :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> Config -> Sitemap -> CtrlV
routeWithOptions routeAuthenticate config url = do
    m <- lift getHttpMethod
    if m == OPTIONS
        then lift $ corsResponse (cfCors config)
        else addCorsToResponse (cfCors config) $ authOrRoute routeAuthenticate url
