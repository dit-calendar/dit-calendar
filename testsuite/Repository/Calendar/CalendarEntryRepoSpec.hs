module Repository.Calendar.CalendarEntryRepoSpec (spec) where

import Test.Hspec

import Data.Maybe           ( isJust, fromJust)
import Data.Acid            ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception    ( bracket )
import Data.IxSet           ( IxSet(..), insert, empty )

import Repository.Calendar.CalendarEntryRepo as CalendarEntryRepo
import Domain.Calendar.CalendarEntry         as CalendarEntry

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState CalendarEntryRepo.EntryList -> IO ()) -> IO ()
withDatabaseConnection = 
    bracket (openLocalState CalendarEntryRepo.EntryList{
      nextEntryId = 1,
      entrySet = insert CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0 } empty })
            closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "find" $ do
              it "by id" $
                \c -> do
                  entryState <- query c $ CalendarEntryRepo.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Foo", entryId=0 }

              it "all and check length" $
                \c -> do
                  entryList <- query c GetEntryList
                  let entryCount = nextEntryId entryList
                  entryState <- query c CalendarEntryRepo.AllEntrys
                  length entryState `shouldBe` entryCount

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList <- query c GetEntryList
                  _ <- update c (NewEntry "Mike") 
                  entryState <- query c $ CalendarEntryRepo.EntryById $ nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Mike", entryId=nextEntryId entryList}

              it "new and check nextEntryId" $
                \c -> do
                  entryList <- query c GetEntryList
                  let oldId = nextEntryId entryList
                  _ <- update c (NewEntry "Mike") 
                  entryList <- query c GetEntryList
                  nextEntryId entryList `shouldBe` oldId + 1

