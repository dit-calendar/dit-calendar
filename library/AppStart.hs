module AppStart where

import Control.Monad.Reader  ( runReaderT )
import Happstack.Server      ( ServerPartT(..), mapServerPartT, nullConf, simpleHTTP )

import Controller.AcidHelper        ( withAcid, Acid, App(..) )
import Route.PageEnum      as PageEnum
import Route.Routing       as Route


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) =
    mapServerPartT (`runReaderT` acid) sp

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run =
    withAcid Nothing $ \acid ->
        -- TODO read url and pars it to SiteMap
        simpleHTTP nullConf $ runApp acid $ Route.route PageEnum.Home
