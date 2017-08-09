module Data.Repository.Acid.DataBaseHelper (createDatabaseConnection) where

import System.Directory     ( removeDirectoryRecursive )

import Data.Acid            ( AcidState, IsAcidic, openLocalStateFrom, closeAcidState )
import Control.Exception    ( bracket )

basePath :: String
basePath = "testsuite/temptestdb"

removeDataBaseDirectory :: IO ()
removeDataBaseDirectory = removeDirectoryRecursive basePath

createDatabaseConnection :: IsAcidic a => a -> (AcidState a -> IO ()) -> IO ()
createDatabaseConnection taskAcid =
    bracket (openLocalStateFrom basePath taskAcid)
    $ \c -> (>>) (closeAcidState c) removeDataBaseDirectory
