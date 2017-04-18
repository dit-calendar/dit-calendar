{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies,
  RecordWildCards #-}

module Repository.CalendarRepo where

import Prelude                  hiding ( head )

import Control.Applicative             ( (<$>) )
import Control.Monad.Reader            ( ask )
import Control.Monad.State             ( get, put )
import Data.Data                       ( Data, Typeable )
import Data.Maybe                      ( fromJust )
import Data.Acid                       ( Query, Update, makeAcidic )
import Data.SafeCopy                   ( base, deriveSafeCopy )
import Data.IxSet                      ( Indexable(..), IxSet(..), (@=)
                                       , Proxy(..), getOne, ixFun, ixSet
                                       , toList, getEQ, insert )

import Domain.CalendarEntry            ( CalendarEntry(..) )
import Domain.Types                    ( EntryId, UserId )


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ] ]


--type that represents the state we wish to store
data EntryList = EntryList
    { nextEntryId           :: EntryId
    , entrySet              :: IxSet CalendarEntry
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''EntryList)

initialEntryListState :: EntryList
initialEntryListState =
    EntryList { 
        nextEntryId              = 1
        , entrySet               = empty
        }

getEntryList :: Query EntryList EntryList
getEntryList = ask

-- create a new entry and add it to the database
newEntry :: String -> UserId -> Update EntryList CalendarEntry
newEntry n userId =
    do  b@EntryList{..} <- get
        let nexCalendarEntryId = nextEntryId
            entry = CalendarEntry { 
                        description    = n
                        , entryId      = nexCalendarEntryId
                        , userId       = userId
                        }
        --Because EntryId is an instance of Enum we can use succ to increment it.
        put $ b { nextEntryId          = succ nextEntryId
                , entrySet             = insert entry entrySet
                }
        return entry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById eid = getOne . getEQ eid . entrySet <$> ask

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = toList . entrySet <$> ask

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList])
