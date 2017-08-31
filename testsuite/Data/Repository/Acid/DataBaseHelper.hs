{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.Acid.DataBaseHelper ( initDatabase ) where

import System.Directory     ( removeDirectoryRecursive )
import Data.Data            ( Typeable )

import Data.Acid            ( AcidState, IsAcidic, openLocalStateFrom, closeAcidState )
import Control.Exception    ( bracket )
import Data.IxSet           ( Indexable(..), insert, empty )

import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid

basePath :: String
basePath = "testsuite/temptestdb"

removeDataBaseDirectory :: IO ()
removeDataBaseDirectory = removeDirectoryRecursive basePath

createDatabaseConnection :: IsAcidic a => a -> (AcidState a -> IO ()) -> IO ()
createDatabaseConnection taskAcid =
    bracket (openLocalStateFrom basePath taskAcid)
    $ \c -> (>>) (closeAcidState c) removeDataBaseDirectory

initDatabase :: (IsAcidic (InterfaceAcid.EntrySet a), Typeable a, Ord a, Indexable a) =>
    a -> (AcidState (InterfaceAcid.EntrySet a) -> IO ()) -> IO ()
initDatabase initEntry = createDatabaseConnection InterfaceAcid.EntrySet{
        InterfaceAcid.nextEntryId   = 1
        , InterfaceAcid.entrys      = insert initEntry empty
        }
