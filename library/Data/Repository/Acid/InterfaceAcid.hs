{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}

module Data.Repository.Acid.InterfaceAcid where

import Control.Applicative             ( (<$>) )
import Control.Monad.Reader            ( ask )
import Control.Monad.State             ( get, put )
import Data.Data                       ( Data, Typeable )
import Data.Acid                       ( Query, Update )
import Data.SafeCopy                   ( base, deriveSafeCopy )
import Data.IxSet                      ( Indexable(..), IxSet(..), getOne
                                       , toList, getEQ, deleteIx, updateIx )


--type that represents the state we wish to store
data (Typeable a, Ord a, Indexable a) => EntrySet a = EntrySet
    { nextEntryId           :: Int
    , entrys                :: IxSet a
    }
    deriving (Data, Typeable)

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
allEntrysAsList = toList . entrys <$> ask

entryById :: (Ord a, Typeable a, Indexable a) => Int -> Query (EntrySet a) (Maybe a)
entryById eid = getOne . getEQ eid . entrys <$> ask
            
deleteEntry :: (Ord a, Typeable a, Indexable a) => Int -> Update (EntrySet a) ()
deleteEntry entryToDelete =
    do  b@EntrySet{..} <- get
        put $ b { entrys =
            deleteIx entryToDelete entrys
            }

updateEntry :: (Ord a, Typeable a, Indexable a) => a -> (a -> Int) -> Update (EntrySet a) ()
updateEntry updatedEntry getId =
     do b@EntrySet{..} <- get
        put $ b { entrys =
            updateIx (getId updatedEntry) updatedEntry entrys
            }
