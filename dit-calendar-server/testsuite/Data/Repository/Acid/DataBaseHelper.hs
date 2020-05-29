{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.Acid.DataBaseHelper ( initDatabase, initDatabaseWithList ) where

import           Data.Data                          (Typeable)
import           Data.SafeCopy
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

createDatabaseConnection :: (IsAcidic a, SafeCopy a) => a -> (AcidState a -> IO ()) -> IO ()
createDatabaseConnection taskAcid =
    bracket (openLocalStateFrom basePath taskAcid)
    $ \c -> (>>) (closeAcidState c) removeDataBaseDirectory

initDatabase :: (IsAcidic (InterfaceAcid.EntrySet a key), Typeable a, Ord a, Indexable a, SafeCopy a, SafeCopy key) =>
    key -> a -> (AcidState (InterfaceAcid.EntrySet a key) -> IO ()) -> IO ()
initDatabase initKey initEntry = createDatabaseConnection InterfaceAcid.EntrySet{
        InterfaceAcid.nextEntryId   = initKey
        , InterfaceAcid.entrys      = insert initEntry empty
        }

initDatabaseWithList :: (IsAcidic (InterfaceAcid.EntrySet a key), Typeable a, Ord a, Indexable a, SafeCopy a, SafeCopy key) =>
    key -> [a] -> (AcidState (InterfaceAcid.EntrySet a key) -> IO ()) -> IO ()
initDatabaseWithList initKey initEntry = createDatabaseConnection InterfaceAcid.EntrySet{
     InterfaceAcid.nextEntryId   = initKey
     , InterfaceAcid.entrys      = fromList initEntry
     }
