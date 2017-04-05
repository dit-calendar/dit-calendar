{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies,
  RecordWildCards #-}

module Repository.CalendarRepo where

import Prelude                  hiding ( head )

import Control.Applicative      ( (<$>) )
import Control.Monad.Reader     ( ask )
import Control.Monad.State      ( get, put )
import Data.Data                ( Data, Typeable )
import Data.Acid                ( Query, Update, makeAcidic )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.IxSet               ( Indexable(..), IxSet(..), (@=)
                                , Proxy(..), getOne, ixFun, ixSet
                                , toList, getEQ, insert )

import Domain.Calendar              ( Calendar(..) )


instance Indexable Calendar where
  empty = ixSet [ ixFun $ \bp -> [ calendarId bp ] ]

--type that represents the state we wish to store
data CalendarList = CalendarList
    { nextCalendarId :: Int
    , calendarSet      :: IxSet Calendar
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarList)

initialCalendarListState :: CalendarList
initialCalendarListState =
    CalendarList { nextCalendarId = 1
        , calendarSet      = empty
        }

getCalendarList :: Query CalendarList CalendarList
getCalendarList = ask

-- create a new, empty calendar and add it to the database
newCalendar :: String -> Update CalendarList Calendar
newCalendar n =
    do  b@CalendarList{..} <- get
        let calendar = Calendar { description = n
                        , calendarId  = nextCalendarId
                        }
        --Because CalendarId is an instance of Enum we can use succ to increment it.
        put $ b { nextCalendarId = succ nextCalendarId
                , calendarSet      = insert calendar calendarSet
                }
        return calendar

calendarById :: Int -> Query CalendarList (Maybe Calendar)
calendarById uid = getOne . getEQ uid . calendarSet <$> ask

allCalendars :: Query CalendarList [Calendar]
allCalendars = toList . calendarSet <$> ask

$(makeAcidic ''CalendarList ['newCalendar, 'calendarById, 'allCalendars, 'getCalendarList])
