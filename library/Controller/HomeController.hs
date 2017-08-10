module Controller.HomeController where

import Happstack.Server         ( ok, toResponse, Method(GET, POST)
                                , method, nullDir, Request(rqMethod), askRq )

import Controller.UserController as UserController
import Controller.AcidHelper    ( CtrlV )


--handler for homePage
homePage :: CtrlV
homePage = do
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
        UserController.userPage 1
