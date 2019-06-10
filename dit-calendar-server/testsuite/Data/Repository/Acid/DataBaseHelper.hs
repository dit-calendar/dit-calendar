{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.Acid.DataBaseHelper ( initDatabase, initDatabaseWithList ) where

import           Data.Data                          (Typeable)
import           System.Directory                   (removeDirectoryRecursive)

import           Control.Exception                  (bracket)
import           Data.Acid                          (AcidState, IsAcidic,
                                                     closeAcidState,
                                                     openLocalStateFrom)
import           Data.IxSet                         (Indexable (..), empty,
                                                     insert, fromList)

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid

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

initDatabaseWithList :: (IsAcidic (InterfaceAcid.EntrySet a), Typeable a, Ord a, Indexable a) =>
 [a]-> (AcidState (InterfaceAcid.EntrySet a) -> IO ()) -> IO ()
initDatabaseWithList initEntry = createDatabaseConnection InterfaceAcid.EntrySet{
     InterfaceAcid.nextEntryId   = 1
     , InterfaceAcid.entrys      = fromList initEntry
     }
