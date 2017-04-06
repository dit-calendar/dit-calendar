module Repository.Calendar.CalendarRepoSpec (spec) where

import Test.Hspec

import Data.Maybe                 ( isJust, fromJust)
import Data.Acid                  ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception          ( bracket )
import Data.IxSet                 ( IxSet(..), insert, empty )
import qualified Data.Map as Map  ( Map, insert, empty )

import Repository.Calendar.CalendarRepo      as CalendarRepo
import Domain.Calendar.CalendarEntry         as CalendarEntry

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState CalendarRepo.EntryList -> IO ()) -> IO ()
withDatabaseConnection =
  let firstCalendarEntry = CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0 } in
      bracket (openLocalState CalendarRepo.EntryList{
        nextEntryId            = 1
        , entrySet             = insert firstCalendarEntry empty
        , calendarUserRelation = Map.insert 0 0 Map.empty
        })
              closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "find" $ do
              it "by id" $
                \c -> do
                  entryState   <- query c $ CalendarRepo.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Foo", entryId=0 }
                  entryId      <- query c $ CalendarRepo.EntryByUserId 0
                  entryId `shouldSatisfy` isJust
                  fromJust entryId `shouldBe` 0

              it "all and check length" $
                \c -> do
                  entryList      <- query c GetEntryList
                  let entryCount = nextEntryId entryList
                  entryState     <- query c CalendarRepo.AllEntrys
                  length entryState `shouldBe` entryCount

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList   <- query c GetEntryList
                  _           <- update c (NewEntry "Zahnarzt" 1) 
                  entryState  <- query c $ CalendarRepo.EntryById $ nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt", entryId=nextEntryId entryList}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = nextEntryId entryList
                  _            <- update c (NewEntry "Zahnarzt" 1) 
                  entryList    <- query c GetEntryList
                  nextEntryId entryList `shouldBe` oldId + 1

              it "new and check calendarUserRelation" $
                \c -> do
                  entryList    <- query c GetEntryList
                  _            <- update c (NewEntry "Zahnarzt" 123) 
                  entryList    <- query c GetEntryList
                  entryId      <- query c $ CalendarRepo.EntryByUserId 123
                  entryId `shouldSatisfy` isJust
                  fromJust entryId `shouldBe` nextEntryId entryList - 1

