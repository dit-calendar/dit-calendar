{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.CalendarEntry
    ( CalendarDAO(..), initialEntryListState, EntryList(..), NewEntry(..), EntryById(..), AllEntrys(..),
    AllEntriesForUser(..), AllEntriesForUserAndRange(..), GetEntryList(..), UpdateEntry(..), DeleteEntry(..) ) where

import           Control.Monad.Reader               (asks)
import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), Proxy (..),
                                                     getEQ, getRange, ixFun,
                                                     ixSet, toDescList, toList)

import           Data.Domain.CalendarEntry          (CalendarEntry (..))
import           Data.Domain.Types                  (EitherResult, EntryId,
                                                     StartDate,
                                                     UserIdIndex (..))

import qualified Data.Domain.User                   as User
import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ]
                , ixFun $ \bp -> [ UserIdIndex $ owner bp ]
                , ixFun $ \bp -> [ startDate bp]
                , ixFun $ \bp -> [ endDate bp]
                ]

type EntryList = InterfaceAcid.EntrySet CalendarEntry EntryId

initialEntryListState :: EntryList
initialEntryListState = InterfaceAcid.initialState 1

getEntryList :: Query EntryList EntryList
getEntryList = InterfaceAcid.getEntrySet

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = InterfaceAcid.allEntrysAsList

--TODO suche nach mit Predicat? Für suche mit zeitlichen Grenzen
allEntriesForUser :: User.User -> Query EntryList [CalendarEntry]
allEntriesForUser user = asks (toDescList (Proxy :: Proxy StartDate) . getEQ (UserIdIndex $ User.userId user) . InterfaceAcid.entrys)

allEntriesForUserAndRange :: User.User -> StartDate -> StartDate -> Query EntryList [CalendarEntry]
allEntriesForUserAndRange user from to = asks (toList . getRange from to . getEQ (UserIdIndex $ User.userId user) . InterfaceAcid.entrys)

newEntry :: CalendarEntry -> Update EntryList CalendarEntry
newEntry = InterfaceAcid.newEntry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById = InterfaceAcid.entryById

updateEntry :: CalendarEntry -> Update EntryList (EitherResult CalendarEntry)
updateEntry = InterfaceAcid.updateEntry

deleteEntry :: EntryId -> Update EntryList ()
deleteEntry = InterfaceAcid.deleteEntry

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList, 'allEntriesForUser, 'allEntriesForUserAndRange, 'updateEntry, 'deleteEntry])

class Monad m => CalendarDAO m where
    create :: NewEntry -> m CalendarEntry
    update :: UpdateEntry -> m (EitherResult CalendarEntry)
    delete :: DeleteEntry -> m ()
    query  :: EntryById -> m (Maybe CalendarEntry)
    findList   :: AllEntriesForUser -> m [CalendarEntry]
    findListForRange   :: AllEntriesForUserAndRange -> m [CalendarEntry]
