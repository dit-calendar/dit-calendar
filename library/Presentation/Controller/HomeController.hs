module Presentation.Controller.HomeController where

import           Happstack.Server                       (Method (GET, POST),
                                                         Request (rqMethod),
                                                         askRq, method, nullDir,
                                                         ok, toResponse)

import           Presentation.AcidHelper                (CtrlV)
import           Presentation.Controller.UserController as UserController


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
