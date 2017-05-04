module Route.Routing where

import Happstack.Server          ( ok, toResponse, Method(GET, POST, DELETE), nullDir
                                 , Request(rqMethod), askRq , BodyPolicy(..)
                                 , decodeBody, defaultBodyPolicy, look)

import Controller.AcidHelper     ( CtrlV )
import Controller.UserController      as UserController
import Controller.HomeController      as HomeController
import Controller.CalendarController  as CalendarController
import Route.PageEnum            ( SiteMap(..) )
import Data.Domain.Types         ( UserId )


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
  do  decodeBody myPolicy
      case url of
        Home                 -> HomeController.homePage
        Userdetail           -> routeDetailUser
        (User i)             -> routeUser i
        (CalendarEntry i)    -> CalendarController.entryPage i

getHttpMethod = do
  nullDir
  g <- greet
  ok g
    where
  greet = do
    rqMethod <$> askRq

routeUser :: UserId -> CtrlV
routeUser userId = do
  m <- getHttpMethod
  case m of
    GET  ->
      UserController.userPage userId
    DELETE ->
      UserController.deleteUser userId
    POST -> do
      desription <- look "desription"
      CalendarController.createCalendarEntry userId desription

routeDetailUser :: CtrlV
routeDetailUser = do
  m <- getHttpMethod
  case m of
  -- curl -X POST -d "name=FooBar" http://localhost:8000/userdetail
    POST -> do
      name <- look "name"
      UserController.createUser name
