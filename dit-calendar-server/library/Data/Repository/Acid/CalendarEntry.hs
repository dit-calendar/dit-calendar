{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.CalendarEntry
    ( CalendarDAO(..), initialEntryListState, EntryList(..), NewEntry(..), EntryById(..), AllEntrys(..),
    AllEntriesForUser(..), GetEntryList(..), UpdateEntry(..), DeleteEntry(..) ) where

import           Data.Acid                          (Query, Update, makeAcidic)
import           Control.Monad.Reader (ask)
import           Data.IxSet                         (Indexable (..), getEQ,
                                                     ixFun, ixSet, toList)

import           Data.Domain.CalendarEntry          (CalendarEntry (..))
import           Data.Domain.Types                  (EitherResponse, EntryId)

import qualified Data.Domain.User                   as User
import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ]
                , ixFun $ \bp -> [ userId bp ]] --TODO index über UserId. Momentan sitz zwei indexe über entryId

type EntryList = InterfaceAcid.EntrySet CalendarEntry

initialEntryListState :: EntryList
initialEntryListState = InterfaceAcid.initialState

getEntryList :: Query EntryList EntryList
getEntryList = InterfaceAcid.getEntrySet

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = InterfaceAcid.allEntrysAsList

--TODO sortieren nach Date
--TODO suche nach mit Predicat? Für suche mit zeitlichen Grenzen
allEntriesForUser :: User.User -> Query EntryList [CalendarEntry]
allEntriesForUser user =  toList . getEQ (User.userId user) . InterfaceAcid.entrys <$> ask

newEntry :: CalendarEntry -> Update EntryList CalendarEntry
newEntry = InterfaceAcid.newEntry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById = InterfaceAcid.entryById

updateEntry :: CalendarEntry -> Update EntryList (EitherResponse CalendarEntry)
updateEntry = InterfaceAcid.updateEntry

deleteEntry :: EntryId -> Update EntryList ()
deleteEntry = InterfaceAcid.deleteEntry

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList, 'allEntriesForUser, 'updateEntry, 'deleteEntry])

class Monad m => CalendarDAO m where
    create :: NewEntry -> m CalendarEntry
    update :: UpdateEntry -> m (EitherResponse CalendarEntry)
    delete :: DeleteEntry -> m ()
    query  :: EntryById -> m (Maybe CalendarEntry)
    findList   :: AllEntriesForUser -> m [CalendarEntry]
