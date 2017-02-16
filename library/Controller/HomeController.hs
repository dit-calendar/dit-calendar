module Controller.HomeController where

import Controller.UserController as UserController
import Controller.AcidHelper
import Happstack.Server  ( ok, toResponse, lookRead, Method(GET, POST), method
                           , nullDir, Request(rqMethod), askRq )

--handler for homePage
getHomePage :: CtrlV
getHomePage = do
  nullDir
  g <- greet
  ok $ toResponse (show g)
    where
  greet = do
    m <- rqMethod <$> askRq
    case m of
    -- curl http://localhost:8000/home-page
      GET  -> do
        getUsersPage
    -- curl -d '' http://localhost:8000/home-page
      POST  -> do
        getUserPage 1
