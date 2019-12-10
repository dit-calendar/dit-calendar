module Presentation.Controller.HomeController where

import           Happstack.Server       (Method (GET, POST), Request (rqMethod),
                                         Response, askRq, nullDir, ok,
                                         toResponse)

import           AppContext             (App, getCurrentUser)
import           Server.ResponseBuilder (notImplemented)


--handler for homePage
homePage :: App Response
homePage = do
  nullDir
  g <- greet
  ok $ toResponse (show g)
    where
  greet = do
    m <- rqMethod <$> askRq
    user <- getCurrentUser
    case m of
    -- curl -d '' http://localhost:8000/home
      other -> ok $ toResponse $ "userId ist: " ++ show user
