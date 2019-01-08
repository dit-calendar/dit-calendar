{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.CalendarEntry
    ( CalendarDAO(..), initialEntryListState, EntryList(..), NewEntry(..), EntryById(..), AllEntrys(..),
    GetEntryList(..), UpdateEntry(..), DeleteEntry(..), UpdateEntryAndCheckVersion(..) ) where

import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), ixFun,
                                                     ixSet)

import           Data.Domain.CalendarEntry          (CalendarEntry (..))
import           Data.Domain.Types                  (EntryId)

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ] ]

type EntryList = InterfaceAcid.EntrySet CalendarEntry

initialEntryListState :: EntryList
initialEntryListState = InterfaceAcid.initialState

getEntryList :: Query EntryList EntryList
getEntryList = InterfaceAcid.getEntrySet

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = InterfaceAcid.allEntrysAsList

newEntry :: CalendarEntry -> Update EntryList CalendarEntry
newEntry = InterfaceAcid.newEntry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById = InterfaceAcid.entryById

updateEntry :: CalendarEntry -> Update EntryList ()
updateEntry = InterfaceAcid.updateEntry

updateEntryAndCheckVersion :: CalendarEntry -> Update EntryList (Either String ())
updateEntryAndCheckVersion = InterfaceAcid.updateEntryAndCheckVersion

deleteEntry :: EntryId -> Update EntryList ()
deleteEntry = InterfaceAcid.deleteEntry

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList, 'updateEntry, 'updateEntryAndCheckVersion,
    'deleteEntry])

class Monad m => CalendarDAO m where
    create :: NewEntry -> m CalendarEntry
    update :: UpdateEntry -> m ()
    updateAndCheckVersion :: UpdateEntryAndCheckVersion -> m (Either String ())
    delete :: DeleteEntry -> m ()
    query  :: EntryById -> m (Maybe CalendarEntry)
