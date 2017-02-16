module AppStart where

import Data.Text                    ( pack )
import Happstack.Foundation

import Controller.AcidHelper        ( withAcid )
import Route.PageEnum as PageEnum
import Route.Routing as Route


--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run = withAcid Nothing $ \acid ->
         simpleApp id
            defaultConf
            (AcidUsing acid)
            ()
            PageEnum.Home
            (pack "")
            Route.route
