{-# LANGUAGE DatatypeContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Repository.Acid.InterfaceAcid where

import           Control.Monad.Reader (asks, ask)
import           Control.Monad.State  (get, put)
import           Data.Acid            (Query, Update)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable (..), IxSet (..), deleteIx,
                                       getEQ, getOne, insert, toList, updateIx)
import           Data.Maybe           (fromJust)
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Data.Domain.Types    (EitherResult, Entity,
                                       ResultError (..), getId, getVersion,
                                       setId, setVersion)


--type that represents the state we wish to store
data (Typeable a, Ord a, Indexable a) => EntrySet a = EntrySet
    { nextEntryId :: Int
    , entrys      :: IxSet a
    }
    deriving (Data, Typeable)

incVersion :: Entity a => a -> a
incVersion x = setVersion x (getVersion x + 1)

$(deriveSafeCopy 0 'base ''EntrySet)

initialState :: (Ord a, Typeable a, Indexable a) => EntrySet a
initialState =
    EntrySet {
        nextEntryId              = 1
        , entrys                 = empty
        }

getEntrySet :: (Ord a, Indexable a) => Query (EntrySet a) (EntrySet a)
getEntrySet = ask

allEntrysAsList :: (Ord a, Typeable a, Indexable a) => Query (EntrySet a) [a]
allEntrysAsList = asks (toList . entrys)

entryById :: (Ord a, Typeable a, Indexable a) => Int -> Query (EntrySet a) (Maybe a)
entryById eid = asks (getOne . getEQ eid . entrys)

deleteEntry :: (Ord a, Typeable a, Indexable a) => Int -> Update (EntrySet a) ()
deleteEntry entryToDelete =
    do  b@EntrySet{..} <- get
        put b { entrys =
            deleteIx entryToDelete entrys
            }

updateEntry :: (Ord a, Typeable a, Indexable a, Entity a) => a -> Update (EntrySet a) (EitherResult a)
updateEntry updatedEntry = do
    b@EntrySet{..} <- get
    let dbEntry = fromJust $ getOne (getEQ (getId updatedEntry) entrys)
    if getVersion dbEntry == getVersion updatedEntry then
        let incrementEntry = incVersion updatedEntry in
        do  put b { entrys =
                    updateIx (getId incrementEntry) incrementEntry entrys
                }
            return $ Right incrementEntry
        else return $ Left OptimisticLocking

-- create a new entry and add it to the database
newEntry :: (Ord a, Typeable a, Indexable a, Entity a) => a -> Update (EntrySet a) a
newEntry entry =
    do  b@EntrySet{..} <- get
        let nEntry = setVersion (setId entry nextEntryId) 0
        --Because UserId is an instance of Enum we can use succ to increment it.
        put b { nextEntryId = succ nextEntryId
                , entrys      = insert nEntry entrys
                }
        return nEntry
