module Data.Repository.Acid.CalendarAcidSpec (spec) where

import Test.Hspec

import Data.Maybe                 ( isJust, fromJust, isNothing)
import Data.Acid                  ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception          ( bracket )
import Data.IxSet                 ( IxSet(..), insert, empty )
import qualified Data.Map as Map  ( Map, insert, empty )

import Data.Repository.Acid.CalendarAcid  as CalendarAcid
import Data.Domain.CalendarEntry          as CalendarEntry

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState CalendarAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection =
  let firstCalendarEntry = CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0, userId=0, calendarTasks=[] } in
      bracket (openLocalState CalendarAcid.EntryList{
        nextEntryId            = 1
        , entrySet             = insert firstCalendarEntry empty
        })
              closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "find" $ do
              it "by EntryId" $
                \c -> do
                  entryState   <- query c $ CalendarAcid.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Foo", entryId=0, userId=0, calendarTasks=[] }

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList   <- query c GetEntryList
                  _           <- update c (NewEntry "Zahnarzt" 0)
                  entryState  <- query c $ CalendarAcid.EntryById $ nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt", entryId=nextEntryId entryList, userId=0, calendarTasks=[]}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = nextEntryId entryList
                  _            <- update c (NewEntry "Zahnarzt" 1)
                  entryList    <- query c GetEntryList
                  nextEntryId entryList `shouldBe` oldId + 1

          describe "delete" $ do
              it "create/delete Entry and check existence" $
                \ c -> do 
                  entryList <- query c GetEntryList
                  _ <- update c (NewEntry "Zahnarzt" 1)
                  entryState <- update c $ CalendarAcid.DeleteEntry $ nextEntryId entryList
                  entryList <- query c GetEntryList
                  entryState <- query c $ CalendarAcid.EntryById $ nextEntryId entryList
                  entryState `shouldSatisfy` isNothing

          describe "update" $ do
              it "change attributes and check changes" $
                \c -> do
                  entryList   <- query c GetEntryList
                  let eId = nextEntryId entryList
                  _           <- update c (NewEntry "Termin 1" 2)
                  entryState  <- query c $ CalendarAcid.EntryById $ eId
                  let updatedEntry = (fromJust entryState) {description = "Termin 2"}
                  _           <- update c $ CalendarAcid.UpdateEntry updatedEntry
                  entryState  <- query c $ CalendarAcid.EntryById $ eId
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Termin 2", entryId=eId, userId=2, calendarTasks=[]}
