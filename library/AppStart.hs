module AppStart where

import Controller.AcidHelper as AcidHelper
import Route.PageEnum as PageEnum
import Happstack.Foundation
import Route.Routing as Route

import Data.Text (pack)

--zu HomePage zu erreichen unter http://localhost:8000
run :: IO ()
run = AcidHelper.withAcid Nothing $ \acid ->
         simpleApp id
            defaultConf
            (AcidUsing acid)
            ()
            PageEnum.HomePage
            (pack "")
            route
