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
withDatabaseConnection = initDatabase 1 def{ CalendarEntry.title="A", CalendarEntry.description= Just "Foo", CalendarEntry.entryId=0, owner=0,
    startDate=dbDate, endDate=dbDate}

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  let newCalendarEntry = def { CalendarEntry.title="A", CalendarEntry.description=Just "Zahnarzt", CalendarEntry.entryId=0, owner=0,
                    startDate=dbDate, endDate=dbDate }
                  newCalendarEntryFromDB <- update c $ NewEntry newCalendarEntry
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  findNewCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  findNewCalendarEntryFromDB `shouldSatisfy` isJust
                  fromJust findNewCalendarEntryFromDB `shouldBe` def{ CalendarEntry.title="A", description=Just "Zahnarzt", entryId=newCalendarEntryId, owner=0,
                        startDate=dbDate, endDate=dbDate}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = InterfaceAcid.nextEntryId entryList
                  let calendarEntry = def { CalendarEntry.title="A", CalendarEntry.description=Just "Zahnarzt", CalendarEntry.entryId=0, owner=1,
                    startDate=dbDate, endDate=dbDate }
                  _            <- update c (NewEntry calendarEntry)
                  entryList    <- query c GetEntryList
                  InterfaceAcid.nextEntryId entryList `shouldBe` oldId + 1
                  let newDate2 = read "2012-11-19 17:51:42.203841 UTC"::UTCTime
                  let newCalendarEntry = def { CalendarEntry.title="A", CalendarEntry.description=Just "Zahnarzt2", CalendarEntry.entryId=0, owner=1,
                    startDate=newDate2, endDate=newDate2 }
                  _            <- update c (NewEntry newCalendarEntry)
                  entryState  <- query c $ CalendarEntryAcid.EntryById (oldId + 1)
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def{ CalendarEntry.title="A", description=Just "Zahnarzt2", entryId=oldId + 1, owner=1,
                        startDate=newDate2, endDate=newDate2}

          describe "find" $
              it "by EntryId" $
                \c -> do
                  entryState   <- query c $ CalendarEntryAcid.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def{ title="A", description=Just "Foo", entryId=0, owner=0,
                      startDate=dbDate, endDate=dbDate }

          describe "delete" $
              it "create/delete Entry and check existence" $
                \ c -> do
                  let newCalendarEntry = def { CalendarEntry.title="A", CalendarEntry.description=Just "Zahnarzt", CalendarEntry.entryId=0, owner=1,
                    startDate=dbDate, endDate=dbDate }
                  newCalendarEntryFromDB <- update c (NewEntry newCalendarEntry)
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  _ <- update c $ CalendarEntryAcid.DeleteEntry newCalendarEntryId
                  entryState <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  entryState `shouldSatisfy` isNothing

          describe "update" $ do
              it "change attributes and check changes" $
                \c -> do
                  --erstelle Objekt
                  let newCalendarEntry = def { CalendarEntry.title="A", CalendarEntry.description=Just "Termin 1", CalendarEntry.entryId=0, owner=2,
                    startDate=dbDate, endDate=dbDate }
                  newCalendarEntryFromDB      <- update c (NewEntry newCalendarEntry)
                  let newCalendarEntryId = CalendarEntry.entryId newCalendarEntryFromDB
                  -- update Objekt
                  findNewCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  let updatedEntry = (fromJust findNewCalendarEntryFromDB) {description = Just "Termin 2"}
                  _                           <- update c $ CalendarEntryAcid.UpdateEntry updatedEntry
                  -- überprüfe welche Werte sich gändert haben
                  entryState                  <- query c $ CalendarEntryAcid.EntryById newCalendarEntryId
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` def { CalendarEntry.title="A", description=Just "Termin 2", entryId=newCalendarEntryId, owner=2, version=1,
                    startDate=dbDate, endDate=dbDate}
              it "change data and check version" $
                \ c -> do
                    findCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById 0
                    _                           <- update c $ CalendarEntryAcid.UpdateEntry $ fromJust findCalendarEntryFromDB
                    entryState                  <- query c $ CalendarEntryAcid.EntryById 0
                    fromJust entryState `shouldBe` def { CalendarEntry.title="A", description=Just "Foo", entryId=0, owner=0, version=1,
                        startDate=dbDate, endDate=dbDate}

                    let updatedEntry = (fromJust findCalendarEntryFromDB) {version = 1}
                    _                           <- update c $ CalendarEntryAcid.UpdateEntry updatedEntry
                    entryState                  <- query c $ CalendarEntryAcid.EntryById 0
                    fromJust entryState `shouldBe` def { CalendarEntry.title="A", description=Just "Foo", entryId=0, owner=0, version=2,
                        startDate=dbDate, endDate=dbDate}
              it "change data with wrong version" $
                \ c -> do
                    findCalendarEntryFromDB  <- query c $ CalendarEntryAcid.EntryById 0
                    _                           <- update c $ CalendarEntryAcid.UpdateEntry $ fromJust findCalendarEntryFromDB
                    entryState                  <- query c $ CalendarEntryAcid.EntryById 0
                    fromJust entryState `shouldBe` def { CalendarEntry.title="A", description=Just "Foo", entryId=0, owner=0, version=1,
                        startDate=dbDate, endDate=dbDate}
                    _                           <- update c $ CalendarEntryAcid.UpdateEntry $ fromJust findCalendarEntryFromDB
                    entryState                  <- query c $ CalendarEntryAcid.EntryById 0
                    fromJust entryState `shouldBe` def { CalendarEntry.title="A", description=Just "Foo", entryId=0, owner=0, version=1,
                        startDate=dbDate, endDate=dbDate}