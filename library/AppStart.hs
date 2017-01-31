{-# LANGUAGE TemplateHaskell #-}
module AppStart where

import Domain.User as Domain
import Controller.Repo
import Route.PageEnum as PageEnum
import Happstack.Foundation
import System.FilePath      ((</>))
import Route.Routing as Route

import Prelude                 hiding ( head )

import Control.Monad           (msum)
import Data.Text
import Happstack.Server
    ( toResponse, simpleHTTP, nullConf,
    seeOther, dir, notFound, seeOther)
import Web.Routes.Happstack    ( implSite )
import Control.Exception    ( bracket )
import Data.Acid            ( openLocalState )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.Maybe           (fromMaybe)
import Data.Text

--run :: IO ()
--run = withAcid Nothing (\acid ->
--    simpleHTTP nullConf $ msum
--    [ dir "favicon.ico" $ notFound (toResponse ())
--    , implSite (pack "http://localhost:8000") (pack "/route") $ Route.site
--    , seeOther "/r" (toResponse ())])

--zu HomePage zu erreichen unter http://localhost:8000/route
run :: IO ()
run = withAcid Nothing $ \acid -> do
         simpleApp id
            defaultConf
            (AcidUsing acid)
            ()
            PageEnum.HomePage
            (pack "")
            route

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath
        countPath = basePath </> "userlist"
    in bracket (openLocalStateFrom countPath  Domain.initialUserListState) (createCheckpointAndClose) $ \paste ->
        f (Acid paste)
