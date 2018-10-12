{-# LANGUAGE OverloadedStrings #-}

module Data.Repository.Acid.InterfaceAcidSpec (spec) where

import Test.Hspec
import Data.Maybe                 ( isJust, fromJust, isNothing)
import Data.Acid                  ( AcidState, query, update )

import Data.Repository.Acid.DataBaseHelper   ( initDatabase )
import Data.Repository.Acid.CalendarEntry    ( NewEntry(..), GetEntryList(..) )
import Data.Domain.CalendarEntry               as CalendarEntry
import Data.Time.Clock            ( UTCTime )

import qualified Data.Repository.Acid.CalendarEntry      as   CalendarEntryAcid
import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


dbDate = (read "2011-11-19 18:28:52.607875 UTC")::UTCTime
withDatabaseConnection :: (AcidState CalendarEntryAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0, userId=0, tasks=[], date=dbDate }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList   <- query c GetEntryList
                  let calendarEntry = CalendarEntry { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=0, tasks=[], date=dbDate }
                  _           <- update c (NewEntry calendarEntry)
                  entryState  <- query c $ CalendarEntryAcid.EntryById $ InterfaceAcid.nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt", entryId=InterfaceAcid.nextEntryId entryList, userId=0, tasks=[], date=dbDate}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = InterfaceAcid.nextEntryId entryList
                  let calendarEntry = CalendarEntry { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=1, tasks=[], date=dbDate }
                  _            <- update c (NewEntry calendarEntry)
                  entryList    <- query c GetEntryList
                  InterfaceAcid.nextEntryId entryList `shouldBe` oldId + 1
                  let newDate2 = (read "2012-11-19 17:51:42.203841 UTC")::UTCTime
                  let calendarEntry = CalendarEntry { CalendarEntry.description="Zahnarzt2", CalendarEntry.entryId=0, userId=1, tasks=[], date=newDate2 }
                  _            <- update c (NewEntry calendarEntry)
                  entryState  <- query c $ CalendarEntryAcid.EntryById (oldId + 1)
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt2", entryId=oldId + 1, userId=1, tasks=[], date=newDate2}

          describe "find" $
              it "by EntryId" $
                \c -> do
                  entryState   <- query c $ CalendarEntryAcid.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Foo", entryId=0, userId=0, tasks=[], date=dbDate }

          describe "delete" $
              it "create/delete Entry and check existence" $
                \ c -> do 
                  entryList <- query c GetEntryList
                  let calendarEntry = CalendarEntry { CalendarEntry.description="Zahnarzt", CalendarEntry.entryId=0, userId=1, tasks=[], date=dbDate }
                  _ <- update c (NewEntry calendarEntry)
                  entryState <- update c $ CalendarEntryAcid.DeleteEntry $ InterfaceAcid.nextEntryId entryList
                  entryList <- query c GetEntryList
                  entryState <- query c $ CalendarEntryAcid.EntryById $ InterfaceAcid.nextEntryId entryList
                  entryState `shouldSatisfy` isNothing

          describe "update" $
              it "change attributes and check changes" $
                \c -> do
                  entryList   <- query c GetEntryList
                  let eId = InterfaceAcid.nextEntryId entryList
                  let calendarEntry = CalendarEntry { CalendarEntry.description="Termin 1", CalendarEntry.entryId=0, userId=2, tasks=[], date=dbDate }
                  _           <- update c (NewEntry calendarEntry)
                  entryState  <- query c $ CalendarEntryAcid.EntryById eId
                  let updatedEntry = (fromJust entryState) {description = "Termin 2"}
                  _           <- update c $ CalendarEntryAcid.UpdateEntry updatedEntry
                  entryState  <- query c $ CalendarEntryAcid.EntryById eId
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Termin 2", entryId=eId, userId=2, tasks=[], date=dbDate}
