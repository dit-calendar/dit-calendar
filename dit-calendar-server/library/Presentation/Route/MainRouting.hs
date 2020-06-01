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

import           AppContext                               (App, AppContext,
                                                           CtrlV, getConfig)
import           Conf.Config                              (AppConfig (..))
import           Presentation.Route.CalendarRoute         (routeCalendarEntry, routeCalendarEntryDetails,
                                                           routeCalendarFilter,
                                                           routeCalendarTelegramLinks)
import           Presentation.Route.PageEnum              (Sitemap (..))
import           Presentation.Route.TaskRoute             (routeTask,
                                                           routeTaskDetail,
                                                           routeTaskWithTelegramLink)
import           Presentation.Route.UserRoute             (routeDetailUser)
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
        -- calendar routing
        CalendarEntry              -> routeCalendarEntry
        CalendarFilter             -> routeCalendarFilter
        CalendarEntryDetail eId    -> routeCalendarEntryDetails eId
        CalendarTelegramLinks eId  -> routeCalendarTelegramLinks eId
        -- task routing
        CalendarTask eId           -> routeTask eId
        CalendarTaskDetail eId tId -> routeTaskDetail eId tId
        TaskWithTelegramLink tId   -> routeTaskWithTelegramLink tId

routeWithOptions :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response) -> Sitemap -> CtrlV
routeWithOptions routeAuthenticate url = do
    config <- getConfig
    m <- lift getHttpMethod
    if m == OPTIONS
        then lift $ corsResponse (appConfigCors config)
        else addCorsToResponse (appConfigCors config) $ authOrRoute routeAuthenticate url
