{-# LANGUAGE OverloadedStrings #-}

module Data.Repository.Acid.CalendarSpec (spec) where

import           Data.Acid                           (AcidState, query, update)
import           Data.Default                        (def)
import           Data.Maybe                          (fromJust, isJust,
                                                      isNothing)
import           Data.Time.Clock                     (UTCTime)
import           Test.Hspec

import           Data.Domain.CalendarEntry           as Calendar
import           Data.Domain.User                    as User
import           Data.Repository.Acid.DataBaseHelper (initDatabaseWithList)

import qualified Data.Repository.Acid.CalendarEntry  as CalendarEntryAcid
import qualified Data.Repository.Acid.InterfaceAcid  as InterfaceAcid

withDatabaseConnection :: (AcidState CalendarEntryAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection = initDatabaseWithList 1 [initCalendar, initCalendar2, initCalendar3]

initUser :: User.User
initUser = def{User.userId = 1}

newDate = read "2012-11-19 17:51:42.203841 UTC"::UTCTime

initCalendar :: Calendar.CalendarEntry
initCalendar = def{ Calendar.title="A", Calendar.entryId = 0, Calendar.owner = User.userId initUser, Calendar.description = Just "erster eintrag",
    Calendar.startDate = newDate, Calendar.endDate = newDate}

initCalendar2 :: Calendar.CalendarEntry
initCalendar2 = def{ Calendar.title="A", Calendar.entryId = 1, Calendar.owner = User.userId initUser, Calendar.description = Just "zweiter eintrag",
    Calendar.startDate = newDate, Calendar.endDate = newDate}

initCalendar3 :: Calendar.CalendarEntry
initCalendar3 = def{ Calendar.title="A", Calendar.entryId = 2, Calendar.owner = -1, Calendar.description = Just "sollte nicht angezeigt werden",
    Calendar.startDate = newDate, Calendar.endDate = newDate}

spec :: Spec
spec =
    around withDatabaseConnection $
        context "Calendar" $
          describe "find" $
              it "all of user" $
                \c -> do
                  calendars   <- query c $ CalendarEntryAcid.AllEntriesForUser initUser
                  calendars `shouldBe` [initCalendar, initCalendar2]
