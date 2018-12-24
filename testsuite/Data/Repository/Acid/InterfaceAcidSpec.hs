{-# LANGUAGE OverloadedStrings #-}

module Data.Repository.Acid.InterfaceAcidSpec (spec) where

import           Data.Acid                           (AcidState, query, update)
import           Data.Default                        (def)
import           Data.Maybe                          (fromJust, isJust,
                                                      isNothing)
import           Test.Hspec

import           Data.Domain.CalendarEntry           as CalendarEntry
import           Data.Repository.Acid.CalendarEntry  (GetEntryList (..),
                                                      NewEntry (..))
import           Data.Repository.Acid.DataBaseHelper (initDatabase)
import           Data.Time.Clock                     (UTCTime)

import qualified Data.Repository.Acid.CalendarEntry  as CalendarEntryAcid
import qualified Data.Repository.Acid.InterfaceAcid  as InterfaceAcid


dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
withDatabaseConnection :: (AcidState CalendarEntryAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase def{ CalendarEntry.description="Foo", CalendarEntry.entryId=0, userId=0, date=dbDate }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  let newCalendarEntry = def { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=0, date=dbDate }
                  newCalendarEntryFromDB <- update c $ NewEntry newCalendarEntry
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  findNewCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  findNewCalendarEntryFromDB `shouldSatisfy` isJust
                  fromJust findNewCalendarEntryFromDB `shouldBe` def{ description="Zahnarzt", entryId=newCalendarEntryId, userId=0, date=dbDate}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = InterfaceAcid.nextEntryId entryList
                  let calendarEntry = def { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=1, date=dbDate }
                  _            <- update c (NewEntry calendarEntry)
                  entryList    <- query c GetEntryList
                  InterfaceAcid.nextEntryId entryList `shouldBe` oldId + 1
                  let newDate2 = read "2012-11-19 17:51:42.203841 UTC"::UTCTime
                  let newCalendarEntry = def { CalendarEntry.description="Zahnarzt2", CalendarEntry.entryId=0, userId=1, date=newDate2 }
                  _            <- update c (NewEntry newCalendarEntry)
                  entryState  <- query c $ CalendarEntryAcid.EntryById (oldId + 1)
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def{ description="Zahnarzt2", entryId=oldId + 1, userId=1, date=newDate2}

          describe "find" $
              it "by EntryId" $
                \c -> do
                  entryState   <- query c $ CalendarEntryAcid.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def{ description="Foo", entryId=0, userId=0, date=dbDate }

          describe "delete" $
              it "create/delete Entry and check existence" $
                \ c -> do
                  let newCalendarEntry = def { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=1, date=dbDate }
                  newCalendarEntryFromDB <- update c (NewEntry newCalendarEntry)
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  _ <- update c $ CalendarEntryAcid.DeleteEntry newCalendarEntryId
                  entryState <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  entryState `shouldSatisfy` isNothing

          describe "update" $
              it "change attributes and check changes" $
                \c -> do
                  --erstelle Objekt
                  let newCalendarEntry = def { CalendarEntry.description="Termin 1", CalendarEntry.entryId=0, userId=2, date=dbDate }
                  newCalendarEntryFromDB      <- update c (NewEntry newCalendarEntry)
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  -- update Objekt
                  findNewCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  let updatedEntry = (fromJust findNewCalendarEntryFromDB) {description = "Termin 2"}
                  _                           <- update c $ CalendarEntryAcid.UpdateEntry updatedEntry
                  -- überprüfe welche Werte sich gändert haben
                  entryState                  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def { description="Termin 2", entryId=newCalendarEntryId, userId=2, date=dbDate}
