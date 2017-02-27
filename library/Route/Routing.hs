module Route.Routing where

import Controller.AcidHelper     ( CtrlV )
import Controller.UserController ( userPage )
import Controller.HomeController ( homePage )
import Route.PageEnum            ( SiteMap(..) )
import Happstack.Server         ( ok, toResponse, lookRead, Method(GET, POST)
                                , method, nullDir, Request(rqMethod), askRq )

import Controller.UserController as UserController


-- | the route mapping function
route :: SiteMap -> CtrlV
route url =
    case url of
      Home     -> homePage
      Userdetail     -> routeUser
      (User i) -> userPage i


routeUser :: CtrlV
routeUser = do
  nullDir
  g <- greet
  ok $ toResponse (show g)
    where
  greet = do
    m <- rqMethod <$> askRq
    case m of
    -- curl http://localhost:8000/home
      GET  ->
        UserController.usersPage
    -- curl -d '' http://localhost:8000/home
      POST  ->
        UserController.createUser "test"