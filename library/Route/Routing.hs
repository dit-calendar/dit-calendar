module Route.Routing where

import Happstack.Server         ( ok, toResponse, Method(GET, POST), nullDir
                                , Request(rqMethod), askRq , BodyPolicy(..)
                                , decodeBody, defaultBodyPolicy, look)
import Controller.AcidHelper     ( CtrlV )

import Controller.UserController as UserController
import Controller.HomeController as HomeController
import Route.PageEnum            ( SiteMap(..) )


myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
  do  decodeBody myPolicy
      case url of
        Home        -> HomeController.homePage
        Userdetail  -> routeUser
        (User i)    -> UserController.userPage i

routeUser :: CtrlV
routeUser = do
  nullDir
  g <- greet
  ok g
    where
  greet = do
    m <- rqMethod <$> askRq
    case m of
    -- curl http://localhost:8000/userdetail
      GET  ->
        UserController.usersPage
    -- curl -X POST -d "name=FooBar" http://localhost:8000/userdetail
      POST -> do
        name <- look "name"
        UserController.createUser name