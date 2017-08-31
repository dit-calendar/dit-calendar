{-# LANGUAGE  TemplateHaskell, TypeFamilies,
  RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module Data.Repository.Acid.CalendarAcid 
    (initialEntryListState, EntryList(..), NewEntry(..), EntryById(..), AllEntrys(..),
    GetEntryList(..), UpdateEntry(..), DeleteEntry(..)) where


import Control.Monad.State             ( get, put )
import Data.Acid                       ( Query, Update, makeAcidic )
import Data.IxSet                      ( Indexable(..), ixFun, ixSet, insert )

import Data.Domain.CalendarEntry       ( CalendarEntry(..) )
import Data.Domain.Types               ( EntryId, UserId )

import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ] ]

type EntryList = InterfaceAcid.EntrySet CalendarEntry

initialEntryListState :: EntryList
initialEntryListState = InterfaceAcid.initialState

getEntryList :: Query EntryList EntryList
getEntryList = InterfaceAcid.getEntrySet

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = InterfaceAcid.allEntrysAsList

-- create a new entry and add it to the database
newEntry :: String -> UserId -> Update EntryList CalendarEntry
newEntry n userId =
    do  b@InterfaceAcid.EntrySet{..} <- get
        let nexCalendarEntryId = nextEntryId
            entry = CalendarEntry { 
                        description      = n
                        , entryId        = nexCalendarEntryId
                        , userId         = userId
                        , calendarTasks  = []
                        }
        --Because EntryId is an instance of Enum we can use succ to increment it.
        put $ b { InterfaceAcid.nextEntryId     = succ nextEntryId
                , InterfaceAcid.entrys          = insert entry entrys
                }
        return entry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById = InterfaceAcid.entryById

updateEntry :: CalendarEntry -> Update EntryList ()
updateEntry updatedEntry = InterfaceAcid.updateEntry updatedEntry entryId
            
deleteEntry :: EntryId -> Update EntryList ()
deleteEntry = InterfaceAcid.deleteEntry

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList, 'updateEntry, 'deleteEntry])
